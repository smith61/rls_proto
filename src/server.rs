//! Implements encoding/decoding support and types for the server side of the Language Server protocol.
//!
//! Provides an implementation of tokio_core::io::Codec to decode IncomingServerMessages and encode
//! OutgoingServerMessages

#![allow( non_upper_case_globals )]

use languageserver_types::*;
use serde;
use serde_json;
use std::{
    io,
    mem,
    str
};
use std::collections::{
    HashMap
};
use tokio_core::io::{
    Codec,
    EasyBuf
};

macro_rules! server_messages {
    (
        methods {
            $( $method_name : ident => $method_type : ident $( ( $method_param : ty ) )*; )*
        },
        notifications {
            $( $notif_name : ident => $notif_type : ident $( ( $notif_param : ty ) )*; )*
        }
    ) => {
        #[derive( Debug )]
        pub enum RequestMethod {
            $( $method_type $( ( $method_param ) )*, )*
        }

        #[derive( Debug )]
        pub enum NotificationMethod {
            $( $notif_type $( ( $notif_param ) )*, )*
        }

        pub fn parse_server_message( msg_json_str : &str ) -> io::Result< ServerMessage > {
            let msg_json : serde_json::Value = match serde_json::from_str( msg_json_str ) {
                Ok( msg_json ) => msg_json,
                Err( _ ) => return Err( new_invalid_data_error( "Invalid JSON data." ) )
            };

            if let Some( method_val ) = msg_json.lookup( "method" ) {
                let method = match method_val.as_str( ) {
                    Some( method ) => method,
                    None => return Err( new_invalid_data_error( "Invalid 'method' field, could not parse as String." ) )
                };
                match method {
                    $(
                        $method_name => {
                            let msg_id = match msg_json.lookup( "id" ) {
                                Some( msg_id ) => {
                                    match msg_id.as_i64( ) {
                                        Some( msg_id ) => msg_id,
                                        None => return Err( new_invalid_data_error( "Invalid 'id' field, could not parse as number." ) )
                                    }
                                },
                                None => return Err( new_invalid_data_error( "Missing required 'id' field." ) )
                            };
                            let msg = RequestMessage {
                                id     : msg_id,
                                method : RequestMethod::$method_type $( ( parse_parameters::< $method_param >( &msg_json )? ) )*
                            };
                            Ok( ServerMessage::Request( msg ) )
                        }
                    ),*
                    $(
                        $notif_name => {
                            let notif = NotificationMessage {
                                method : NotificationMethod::$notif_type $( ( parse_parameters::< $notif_param >( &msg_json )? ) )*
                            };
                            Ok( ServerMessage::Notification( notif ) )
                        }
                    ),*
                    _ => Err( new_invalid_data_error( "Invalid request method." ) )
                }
            }
            else {
                return Err( new_invalid_data_error( "Missing required 'method' field." ) );
            }
        }
    };
}

macro_rules! server_responses {
    (
        datas {
            $( $data_type : ident( $data_param : ty ); )*
        },
        errors {
            $( $err_name : ident( $err_val : expr ); )*
        }
    ) => {
        #[derive( Debug )]
        pub enum RequestResponseData {
            $( $data_type ( $data_param ) ),*
        }

        $( pub static $err_name : i64 = $err_val; )*

        impl serde::Serialize for RequestResponseData {

            fn serialize< S >( &self, serializer : &mut S ) -> Result< ( ), S::Error > where S : serde::Serializer {
                match *self {
                    $(
                        RequestResponseData::$data_type( ref param ) => {
                            param.serialize( serializer )
                        }
                    ),*
                }
            }

        }
    }
}

/// A completely parsed message and its headers sent from the client to the server.
#[derive( Debug )]
pub struct IncomingServerMessage {
    /// The headers that accompanied the message.
    pub headers : HashMap< String, String >,

    /// The parsed message.
    pub message : ServerMessage
}

/// A complete message to send from the server to the client.
#[derive( Debug )]
pub struct OutgoingServerMessage {
    /// The headers that should accompany this message.
    ///
    /// Content-Length and Content-Type will be ignored when writing all headers.
    pub headers : HashMap< String, String >,

    /// The message to send to the client.
    pub message : ClientMessage
}

/// Represents either a request or a notification sent from the client to the server.
#[derive( Debug )]
pub enum ServerMessage {
    /// A RequestMessage sent from the client to the server.
    Request( RequestMessage ),

    /// A NotificationMessage sent from the client to the server.
    Notification( NotificationMessage )
}

/// Represents a response from the server to the client.
#[derive( Debug )]
pub enum ClientMessage {
    /// A RequestResponseMessage that contains the response for a client's previously made request.
    Response( RequestResponseMessage )
}

/// A Request from the client to the server.
///
/// Requests must be responded to in the order they were received, but they do not have to be
/// processed in any specific order.
#[derive( Debug )]
pub struct RequestMessage {
    /// The ID of the request that is used to distinguish multiple parallel request's and their responses.
    pub id     : i64,
    /// The method and it's parameters to process.
    pub method : RequestMethod
}

/// A Notification from the client to the server.
///
/// Notifications must not be responded to and can be handled in any order the server sees fit.
#[derive( Debug )]
pub struct NotificationMessage {
    /// The method and it's parameters to process.
    pub method : NotificationMethod
}

/// An error response from the server to the client in response to a specific request.
#[derive( Debug, Serialize )]
pub struct RequestResponseError {
    /// The error code.
    pub code    : i64,
    /// An accompanying error message to present to the client.
    pub message : String
}

/// A message from the server to the client in response to a specific request.
///
/// May contain either a result or an error for either success or failure of a specific client request.
#[derive( Debug )]
pub struct RequestResponseMessage {
    /// The id of the request that this message responds to.
    pub id     : i64,
    /// The result of a successful completion of the request.
    pub result : Option< RequestResponseData >,
    /// The error of a request that failed.
    pub error  : Option< RequestResponseError >
}

enum RLSCodecState {
    Headers,
    Body
}

/// Implements decoding of IncomingServerMessage and encoding of OutgoingServerMessage
///
/// Implements tokio_core::io::Core
pub struct RLSCodec {
    codec_state    : RLSCodecState,
    headers        : HashMap< String, String >,
    content_length : usize
}

server_messages! {
    methods {
        REQUEST__Initialize                        => Initialize( InitializeParams );
        REQUEST__Shutdown                          => Shutdown;
        REQUEST__Completion                        => Completion( TextDocumentPositionParams );
        REQUEST__ResolveCompletionItem             => CompletionResolve( CompletionItem );
        REQUEST__Hover                             => Hover( TextDocumentPositionParams );
        REQUEST__SignatureHelp                     => SignatureHelp( TextDocumentPositionParams );
        REQUEST__GotoDefinition                    => GotoDefinition( TextDocumentPositionParams );
        REQUEST__References                        => FindReferences( ReferenceParams );
        REQUEST__DocumentHighlight                 => DocumentHighlights( TextDocumentPositionParams );
        REQUEST__DocumentSymbols                   => DocumentSymbols( DocumentSymbolParams );
        REQUEST__WorkspaceSymbols                  => WorkspaceSymbols( WorkspaceSymbolParams );
        REQUEST__CodeAction                        => CodeAction( CodeActionParams );
        REQUEST__CodeLens                          => CodeLens( CodeLensParams );
        REQUEST__CodeLensResolve                   => CodeLensResolve( CodeLens );
        REQUEST__DocumentLink                      => DocumentLink( DocumentLinkParams );
        REQUEST__DocumentLinkResolve               => DocumentLinkResolve( DocumentLink );
        REQUEST__Formatting                        => DocumentFormatting( DocumentFormattingParams );
        REQUEST__RangeFormatting                   => DocumentRangeFormatting( DocumentRangeFormattingParams );
        REQUEST__OnTypeFormatting                  => DocumentOnTypeFormatting( DocumentOnTypeFormattingParams );
        REQUEST__Rename                            => Rename( RenameParams );
    },
    notifications {
        NOTIFICATION__Cancel                       => Cancel( CancelParams );
        NOTIFICATION__Exit                         => Exit;
        NOTIFICATION__WorkspaceChangeConfiguration => DidChangeConfiguration( DidChangeConfigurationParams );
        NOTIFICATION__DidOpenTextDocument          => DidOpenTextDocument( DidOpenTextDocumentParams );
        NOTIFICATION__DidChangeTextDocument        => DidChangeTextDocument( DidChangeTextDocumentParams );
        NOTIFICATION__DidCloseTextDocument         => DidCloseTextDocument( DidCloseTextDocumentParams );
        NOTIFICATION__DidSaveTextDocument          => DidSaveTextDocument( DidSaveTextDocumentParams );
        NOTIFICATION__DidChangeWatchedFiles        => DidChangeWatchedFiles( DidChangeWatchedFilesParams );
    }
}

server_responses! {
    datas {
        Init( InitializeResult );
        SymbolInfo( Vec< SymbolInformation > );
        CompletionItems( Vec< CompletionItem > );
        WorkspaceEdit( WorkspaceEdit );
        TextEdit( [ TextEdit; 1 ] );
        Locations( Vec< Location > );
        Highlights( Vec< DocumentHighlight > );
        HoverSuccess( Hover );
    },
    errors {
        PARSE_ERROR( -32700 );
        INVALID_REQUEST( -32600 );
        METHOD_NOT_FOUND( -32601 );
        INVALID_PARAMS( -32602 );
        INTERNAL_ERROR( -32603 );
        SERVER_ERROR_START( -32099 );
        SERVER_ERROR_END( -32000 );
        SERVER_NOT_INITIALIZED( -32002 );
        UNKNOWN_ERROR_CODE( -32001 );
    }
}

fn new_invalid_data_error( error_msg : &'static str ) -> io::Error {
    io::Error::new( io::ErrorKind::InvalidData, error_msg )
}

fn parse_parameters< T >( msg_json : &serde_json::Value ) -> io::Result< T > where T : serde::Deserialize {
    let params = match msg_json.lookup( "params" ) {
        Some( params ) => params.to_owned( ),
        None => return Err( new_invalid_data_error( "Missing required 'params' field." ) )
    };

    match serde_json::from_value( params ) {
        Ok( value ) => Ok( value ),
        Err( _ ) => Err( new_invalid_data_error( "Unable to parse parameter field." ) )
    }
}

fn easybuf_to_utf8( buf : &EasyBuf ) -> io::Result< &str > {
    match str::from_utf8( buf.as_ref( ) ) {
        Ok( s ) => Ok( s ),
        Err( _ ) => Err( new_invalid_data_error( "Unable to parse bytes as UTF-8." ) )
    }
}

impl RLSCodec {

    pub fn new( ) -> Self {
        RLSCodec {
            codec_state    : RLSCodecState::Headers,
            headers        : HashMap::new( ),
            content_length : 0
        }
    }

    fn read_header( &mut self, buf : &mut EasyBuf ) -> io::Result< bool > {
        if let Some( pos ) = buf.as_ref( ).windows( 2 ).position( | bytes | bytes == b"\r\n" ) {
            let header_bytes = buf.drain_to( pos );
            buf.drain_to( 2 );

            if pos == 0 {
                self.content_length = self.parse_content_length( )?;
                self.codec_state    = RLSCodecState::Body;

                trace!( "Finished parsing headers. Waiting for '{}' bytes for body.", self.content_length );
            }
            else {
                let header_str = easybuf_to_utf8( &header_bytes )?;
                trace!( "Received header line: {}", header_str );

                let mut header_parts = header_str.split( ": " );

                if header_parts.clone( ).count( ) != 2 {
                    return Err( new_invalid_data_error( "Unable to parse invalid header." ) );
                }

                let header_key = header_parts.next( ).unwrap( );
                let header_val = header_parts.next( ).unwrap( );

                self.headers.insert( header_key.to_string( ), header_val.to_string( ) );
            }
            Ok( true )
        }
        else {
            Ok( false )
        }
    }

    fn parse_content_length( &self ) -> io::Result< usize > {
        if let Some( content_length_str ) = self.headers.get( "Content-Length" ) {
            if let Ok( content_length ) = content_length_str.parse::< usize >( ) {
                Ok( content_length )
            }
            else {
                Err( new_invalid_data_error( "Unable to parse 'Content-Length' header into usize." ) )
            }
        }
        else {
            Err( new_invalid_data_error( "Missing required 'Content-Length' header." ) )
        }
    }

    fn reset_codec( &mut self ) -> HashMap< String, String > {
        let mut new_headers = HashMap::new( );
        mem::swap( &mut self.headers, &mut new_headers );

        self.codec_state = RLSCodecState::Headers;

        new_headers
    }

    fn parse_body_contents( &mut self, buf : &mut EasyBuf ) -> io::Result< Option< IncomingServerMessage > > {
        if buf.len( ) < self.content_length {
            return Ok( None );
        }

        let body_bytes = buf.drain_to( self.content_length );
        let body_str   = easybuf_to_utf8( &body_bytes )?;
        trace!( "Received body contents: {}", body_str );

        let message = parse_server_message( body_str )?;
        trace!( "Parsed server message: {:?}", message );

        let headers = self.reset_codec( );

        Ok( Some( IncomingServerMessage{
            headers : headers,
            message : message
        } ) )
    }

}

impl Codec for RLSCodec {

    type In  = IncomingServerMessage;
    type Out = OutgoingServerMessage;

    fn decode( &mut self, buf : &mut EasyBuf ) -> io::Result< Option< Self::In > > {
        loop {
            match self.codec_state {
                RLSCodecState::Headers => {
                    if !self.read_header( buf )? {
                        return Ok( None );
                    }
                },
                RLSCodecState::Body => {
                    return self.parse_body_contents( buf );
                }
            }
        }
    }

    fn encode( &mut self, msg : OutgoingServerMessage, buffer : &mut Vec< u8 > ) -> io::Result< ( ) > {
        let OutgoingServerMessage{ mut headers, message } = msg;

        let msg_json = message.into_json( );
        headers.insert( "Content-Length".to_string( ), msg_json.as_bytes( ).len( ).to_string( ) );
        headers.insert( "Content-Type".to_string( ), "application/vscode-jsonrpc; charset=utf8".to_string( ) );

        for ( ref key, ref val ) in headers {
            trace!( "Sending header: {} = {}", key, val );

            buffer.extend_from_slice( key.as_bytes( ) );
            buffer.extend_from_slice( b": " );
            buffer.extend_from_slice( val.as_bytes( ) );
            buffer.extend_from_slice( b"\r\n" );
        }
        buffer.extend_from_slice( b"\r\n" );

        trace!( "Sending body: {}", msg_json );
        buffer.extend_from_slice( msg_json.as_bytes( ) );

        Ok( ( ) )
    }
}

impl ClientMessage {

    fn into_json( self ) -> String {
        match self {
            ClientMessage::Response( value ) => {

                #[derive( Serialize )]
                struct ResponseMessage {
                    jsonrpc : &'static str,
                    id      : i64,
                    result  : Option< RequestResponseData >,
                    error   : Option< RequestResponseError >
                }

                serde_json::to_string( &ResponseMessage {
                    jsonrpc : "2.0",
                    id      : value.id,
                    result  : value.result,
                    error   : value.error
                } ).unwrap( )
            }
        }
    }

}