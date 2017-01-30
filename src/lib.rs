#![allow( non_upper_case_globals )]

#[macro_use]
extern crate log;
extern crate languageserver_types;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate tokio_core;


pub use languageserver_types::*;
use std::{
    fmt,
    io,
    mem,
    str
};
use std::collections::{
    HashMap
};
use std::marker::{
    PhantomData
};
use tokio_core::io::{
    EasyBuf,
    Codec
};

macro_rules! messages {
    (
        message => $message_type : ident,
        requests => $req_enum_name : ident {
            params {
                $( $req_name_p : ident => $req_type_p : ident( $req_param : ty ); )*
            },
            no_params {
                $( $req_name_n : ident => $req_type_n : ident; )*
            }
        },
        responses => $res_enum_name : ident {
            $( $res_type : ident ( $res_param : ty ); )*
        },
        notifications => $notif_enum_name : ident {
            params {
                $( $notif_name_p : ident => $notif_type_p : ident( $notif_param : ty ); )*
            },
            no_params {
                $( $notif_name_n : ident => $notif_type_n : ident; )*
            }
        }
    ) => {

        #[derive( Debug )]
        pub enum $req_enum_name {
            Custom( CustomParams ),
            $( $req_type_p( $req_param ), )*
            $( $req_type_n, )*
        }

        #[derive( Debug )]
        pub enum $res_enum_name {
            Custom( serde_json::Value ),
            $( $res_type( $res_param ) ),*
        }

        #[derive( Debug )]
        pub enum $notif_enum_name {
            Custom( CustomParams ),
            $( $notif_type_p( $notif_param ), )*
            $( $notif_type_n, )*
        }

        impl RequestMessageImpl for $req_enum_name {

            fn get_message_method( &self ) -> &str {
                match *self {
                    $(
                        $req_enum_name::$req_type_p( _ ) => {
                            $req_name_p
                        }
                    ),*
                    $(
                        $req_enum_name::$req_type_n => {
                            $req_name_n
                        }
                    ),*
                    $req_enum_name::Custom( ref params ) => {
                        &params.name
                    }
                }
            }

            fn try_from_json( msg_json : &serde_json::Value ) -> io::Result< RequestMessage< $req_enum_name > > {
                let req_id = lookup_id( msg_json )?;
                let method_name = lookup_method_name( msg_json )?;

                let message = match method_name {
                    $(
                        $req_name_p => {
                            $req_enum_name::$req_type_p( parse_parameters::< $req_param >( msg_json )? )
                        }
                    ),*
                    $(
                        $req_name_n => {
                            $req_enum_name::$req_type_n
                        }
                    ),*
                    method_name => {
                        let params = match msg_json.lookup( "params" ) {
                            Some( params ) => Some( params.to_owned( ) ),
                            None => None
                        };
                        let c_params = CustomParams {
                            name   : method_name.to_owned( ),
                            params : params
                        };
                        $req_enum_name::Custom( c_params )
                    }
                };

                Ok( RequestMessage {
                    id     : req_id,
                    method : message
                } )
            }

        }

        impl serde::Serialize for $req_enum_name {

            fn serialize< S >( &self, serializer : &mut S ) -> Result< ( ), S::Error > where S : serde::Serializer {
                match *self {
                    $(
                        $req_enum_name::$req_type_p( ref param ) => {
                            param.serialize( serializer )
                        }
                    ),*
                    $(
                        $req_enum_name::$req_type_n => {
                            Ok( ( ) )
                        }
                    ),*
                    $req_enum_name::Custom( ref params ) => {
                        if let Some( ref params ) = params.params {
                            params.serialize( serializer )
                        }
                        else {
                            Ok( ( ) )
                        }
                    }
                }
            }

        }

        impl serde::Serialize for $res_enum_name {

            fn serialize< S >( &self, serializer : &mut S ) -> Result< ( ), S::Error > where S : serde::Serializer {
                match *self {
                    $(
                        $res_enum_name::$res_type( ref param ) => {
                            param.serialize( serializer )
                        }
                    ),*
                    $res_enum_name::Custom( ref params ) => {
                        params.serialize( serializer )
                    }
                }
            }

        }

        impl NotificationMessageImpl for $notif_enum_name {

            fn get_message_method( &self ) -> &str {
                match *self {
                    $(
                        $notif_enum_name::$notif_type_p( _ ) => {
                            $notif_name_p
                        }
                    ),*
                    $(
                        $notif_enum_name::$notif_type_n => {
                            $notif_name_n
                        }
                    ),*
                    $notif_enum_name::Custom( ref params ) => {
                        &params.name
                    }
                }
            }

            fn try_from_json( msg_json : &serde_json::Value ) -> io::Result< NotificationMessage< $notif_enum_name > > {
                let method_name = lookup_method_name( msg_json )?;

                let message = match method_name {
                    $(
                        $notif_name_p => {
                            $notif_enum_name::$notif_type_p( parse_parameters::< $notif_param >( msg_json )? )
                        }
                    ),*
                    $(
                        $notif_name_n => {
                            $notif_enum_name::$notif_type_n
                        }
                    ),*
                    method_name => {
                        let params = match msg_json.lookup( "params" ) {
                            Some( params ) => Some( params.to_owned( ) ),
                            None => None
                        };
                        let c_params = CustomParams {
                            name   : method_name.to_owned( ),
                            params : params
                        };
                        $notif_enum_name::Custom( c_params )
                    }
                };

                Ok( NotificationMessage {
                    method : message
                } )
            }

        }

        impl serde::Serialize for $notif_enum_name {

            fn serialize< S >( &self, serializer : &mut S ) -> Result< ( ), S::Error > where S : serde::Serializer {
                match *self {
                    $(
                        $notif_enum_name::$notif_type_p( ref param ) => {
                            param.serialize( serializer )
                        }
                    ),*
                    $(
                        $notif_enum_name::$notif_type_n => {
                            Ok( ( ) )
                        }
                    ),*
                    $notif_enum_name::Custom( ref params ) => {
                        if let Some( ref params ) = params.params {
                            params.serialize( serializer )
                        }
                        else {
                            Ok( ( ) )
                        }
                    }
                }
            }

        }

    };
}

macro_rules! errors {
    (
        $( $err_name : ident( $err_val : expr ); )*
    ) => {
        $( pub static $err_name : i64 = $err_val; )*
    };
}

/// Trait type that encloses types for Requests, Responses, and Notifications.
pub trait Protocol {

    /// The type of requests that are received.
    type RequestType      : RequestMessageImpl;
    /// The type of notifications that are received.
    type NotificationType : NotificationMessageImpl;
    /// The type of responses that are returned from incoming requests.
    type ResponseType     : serde::Serialize + fmt::Debug;

}

/// Trait type that allows parsing and serialization of request type messages.
pub trait RequestMessageImpl : serde::Serialize + fmt::Debug + Sized {

    /// Gets the method string for this request.
    fn get_message_method( &self ) -> &str;

    /// Attempts to parse this request type from the given json value.
    fn try_from_json( msg_json : &serde_json::Value ) -> io::Result< RequestMessage< Self > >;

}

/// Trait type that allows parsing and serialization of notification type messages.
pub trait NotificationMessageImpl : serde::Serialize + fmt::Debug + Sized {

    /// Gets the method string for this notification.
    fn get_message_method( &self ) -> &str;

    /// Attempts to parse this notification type from the given json value.
    fn try_from_json( msg_json : &serde_json::Value ) -> io::Result< NotificationMessage< Self > >;

}

enum LSCodecState {
    Headers,
    Body
}

/// Implements decoding of MessageEnvelope< IncomingMessage< IP::RequestType, IP::NotificationType > >, and
/// encoding of MessageEnvelope< OutgoingMessage< OP::RequestType, OP::NotificationType, IP::ResponseType > >
/// in a tokio_core::io::Codec compliant implementation.
pub struct LSCodec< IP : Protocol, OP : Protocol > {
    codec_state    : LSCodecState,
    headers        : HashMap< String, String >,
    content_length : usize,

    // These just package up types for easy use, but are required to prevent compilation errors.
    __ip           : PhantomData< IP >,
    __op           : PhantomData< OP >
}

/// Contains a message and it's corresponding headers that were either
/// read in or to be written out.
#[derive( Debug )]
pub struct MessageEnvelope< MT : fmt::Debug > {
    /// The headers that accompany the message.
    pub headers : HashMap< String, String >,
    /// The contained message.
    pub message : MT
}

/// An enum type containing messages that can be deserialized.
#[derive( Debug )]
pub enum IncomingMessage< ReqType : RequestMessageImpl, NotifType : NotificationMessageImpl > {
    /// Represents an incoming request.
    Request( RequestMessage< ReqType > ),
    /// Represents an incoming notification.
    Notification( NotificationMessage< NotifType > ),
    /// Represents an incoming reply to a previous request.
    Response( ResponseMessage< serde_json::Value > )
}

/// An enum type containing messages that can be serialized.
#[derive( Debug )]
pub enum OutgoingMessage< ReqType : RequestMessageImpl, NotifType : NotificationMessageImpl, ResType : serde::Serialize + fmt::Debug > {
    /// Represents an outgoing request.
    Request( RequestMessage< ReqType > ),
    /// Represents an outgoing notification.
    Notification( NotificationMessage< NotifType > ),
    /// Represents an outgoing reply to a previous request.
    Response( ResponseMessage< ResType > )
}

/// A language server protocol request.
#[derive( Debug )]
pub struct RequestMessage< RT : RequestMessageImpl > {
    /// The id of the request.
    pub id     : i64,
    /// The method and parameters for the request.
    pub method : RT
}

/// A language server protocol response.
#[derive( Debug )]
pub struct ResponseMessage< RT : serde::Serialize + fmt::Debug > {
    /// The id of the request to response to.
    pub id     : i64,
    /// The result of a successful completion of a request.
    pub result : Option< RT >,
    /// The result of an errored completion of a request.
    pub error  : Option< ResponseError >
}

/// A language server protocol error.
#[derive( Debug, Deserialize, Serialize )]
pub struct ResponseError {
    /// The error code.
    pub code    : i64,
    /// The error message.
    pub message : String
}

/// A language server protocol notification.
#[derive( Debug )]
pub struct NotificationMessage< NT : NotificationMessageImpl > {
    /// The method and parameters of the notification.
    pub method : NT
}

/// Parameters to a custom message that is unrecognized/not implemented.
#[derive( Debug )]
pub struct CustomParams {
    /// The method name.
    pub name   : String,
    /// The optional parameters of the message.
    pub params : Option< serde_json::Value >
}

/// Type representing an incoming message to the server.
pub type IncomingServerMessage = IncomingMessage< ServerRequest, ServerNotification >;
/// Type representing an outgoing message from the server.
pub type OutgoingServerMessage = OutgoingMessage< ClientRequest, ClientNotification, ServerResponse >;

/// Implementation of Protocol for the server.
pub struct ServerProtocol;
impl Protocol for ServerProtocol {

    type RequestType      = ServerRequest;
    type NotificationType = ServerNotification;
    type ResponseType     = ServerResponse;

}
/// Implementation of LSCodec for the server.
pub type ServerCodec = LSCodec< ServerProtocol, ClientProtocol >;


/// Type representing an incoming message to the client.
pub type IncomingClientMessage = IncomingMessage< ClientRequest, ClientNotification >;
/// Type representing an outgoing message from the server.
pub type OutgoingClientMessage = OutgoingMessage< ServerRequest, ServerNotification, ClientResponse >;

/// Implementation of Protocol for the client.
pub struct ClientProtocol;
impl Protocol for ClientProtocol {

    type RequestType      = ClientRequest;
    type NotificationType = ClientNotification;
    type ResponseType     = ClientResponse;

}
/// Implementation of LSCodec for the client.
pub type ClientCodec = LSCodec< ClientProtocol, ServerProtocol >;

messages! {
    message => IncomingServerMessage,
    requests => ServerRequest {
        params {
            REQUEST__Initialize                        => Initialize( InitializeParams );
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
        no_params {
            REQUEST__Shutdown                          => Shutdown;
        }
    },
    responses => ServerResponse {
        Init( InitializeResult );
        SymbolInfo( Vec< SymbolInformation > );
        CompletionItems( Vec< CompletionItem > );
        WorkspaceEdit( WorkspaceEdit );
        TextEdit( [ TextEdit; 1 ] );
        Locations( Vec< Location > );
        Highlights( Vec< DocumentHighlight > );
        HoverSuccess( Hover );
    },
    notifications => ServerNotification {
        params {
            NOTIFICATION__Cancel                       => Cancel( CancelParams );
            NOTIFICATION__WorkspaceChangeConfiguration => DidChangeConfiguration( DidChangeConfigurationParams );
            NOTIFICATION__DidOpenTextDocument          => DidOpenTextDocument( DidOpenTextDocumentParams );
            NOTIFICATION__DidChangeTextDocument        => DidChangeTextDocument( DidChangeTextDocumentParams );
            NOTIFICATION__DidCloseTextDocument         => DidCloseTextDocument( DidCloseTextDocumentParams );
            NOTIFICATION__DidSaveTextDocument          => DidSaveTextDocument( DidSaveTextDocumentParams );
            NOTIFICATION__DidChangeWatchedFiles        => DidChangeWatchedFiles( DidChangeWatchedFilesParams );
        },
        no_params {
            NOTIFICATION__Exit                         => Exit;
        }
    }
}

messages! {
    message => IncomingClientMessage,
    requests => ClientRequest {
        params {
        },
        no_params {
        }
    },
    responses => ClientResponse {
    },
    notifications => ClientNotification {
        params {
            NOTIFICATION__ShowMessage                  => ShowMessage( ShowMessageParams );
            NOTIFICATION__LogMessage                   => LogMessage( LogMessageParams );
            NOTIFICATION__TelemetryEvent               => TelemetryEvent( serde_json::Value );
            NOTIFICATION__PublishDiagnostics           => PublishDiagnostics( PublishDiagnosticsParams );
        },
        no_params {
        }
    }
}

errors! {
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

fn lookup_id( msg_json : &serde_json::Value ) -> io::Result< i64 > {
    match msg_json.lookup( "id" ) {
        Some( req_id ) => {
            match req_id.as_i64( ) {
                Some( req_id ) => Ok( req_id ),
                None => return Err( new_invalid_data_error( "Unable to parse 'id' field as i64." ) )
            }
        },
        None => return Err( new_invalid_data_error( "Missing required 'id' field." ) )
    }
}

fn lookup_method_name( msg_json : &serde_json::Value ) -> io::Result< &str > {
    match msg_json.lookup( "method" ) {
        Some( method_name ) => {
            match method_name.as_str( ) {
                Some( method_name ) => Ok( method_name ),
                None => return Err( new_invalid_data_error( "Unable to parse 'method' field as str." ) )
            }
        },
        None => return Err( new_invalid_data_error( "Missing required 'method' field." ) )
    }
}

fn easybuf_to_utf8( buf : &EasyBuf ) -> io::Result< &str > {
    match str::from_utf8( buf.as_ref( ) ) {
        Ok( s ) => Ok( s ),
        Err( _ ) => Err( new_invalid_data_error( "Unable to parse bytes as UTF-8." ) )
    }
}

impl < IP : Protocol, OP : Protocol > LSCodec< IP, OP > {

    /// Create a new codec instance.
    pub fn new( ) -> Self {
        LSCodec {
            codec_state    : LSCodecState::Headers,
            headers        : HashMap::new( ),
            content_length : 0,

            __ip           : PhantomData,
            __op           : PhantomData
        }
    }

    fn read_header( &mut self, buf : &mut EasyBuf ) -> io::Result< bool > {
        if let Some( pos ) = buf.as_ref( ).windows( 2 ).position( | bytes | bytes == b"\r\n" ) {
            let header_bytes = buf.drain_to( pos );
            buf.drain_to( 2 );

            if pos == 0 {
                self.content_length = self.parse_content_length( )?;
                self.codec_state    = LSCodecState::Body;

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

        self.codec_state = LSCodecState::Headers;

        new_headers
    }

    fn parse_body_contents( &mut self, buf : &mut EasyBuf ) -> io::Result< Option< MessageEnvelope< IncomingMessage< IP::RequestType, IP::NotificationType > > > > {
        if buf.len( ) < self.content_length {
            return Ok( None );
        }

        let body_bytes = buf.drain_to( self.content_length );
        let body_json : serde_json::Value = match serde_json::from_str( easybuf_to_utf8( &body_bytes )? ) {
            Ok( json ) => json,
            Err( _ ) => return Err( new_invalid_data_error( "Invalid JSON data." ) )
        };
        trace!( "Received body contents: {}", body_json );

        let message = {
            if let Ok( request ) = IP::RequestType::try_from_json( &body_json ) {
                IncomingMessage::Request( request )
            }
            else if let Ok( notification ) = IP::NotificationType::try_from_json( &body_json ) {
                IncomingMessage::Notification( notification )
            }
            else if let Some( result ) = body_json.lookup( "result" ) {
                IncomingMessage::Response( {
                    ResponseMessage {
                        id     : lookup_id( &body_json )?,
                        result : Some( result.to_owned( ) ),
                        error  : None
                    }
                } )
            }
            else if let Some( error ) = body_json.lookup( "error" ) {
                let error = match serde_json::from_value( error.to_owned( ) ) {
                    Ok( error ) => error,
                    Err( _ ) => return Err( new_invalid_data_error( "Unable to parse error field." ) )
                };
                IncomingMessage::Response( {
                   ResponseMessage {
                       id     : lookup_id( &body_json )?,
                       result : None,
                       error  : Some( error )
                   }
                } )
            }
            else {
                return Err( new_invalid_data_error( "Could not parse message as request, notification, or response." ) );
            }
        };
        trace!( "Parsed server message: {:?}", message );

        let headers = self.reset_codec( );

        Ok( Some( MessageEnvelope {
            headers : headers,
            message : message
        } ) )
    }

}

impl < IP : Protocol, OP : Protocol > Codec for LSCodec< IP, OP > {

    type In  = MessageEnvelope< IncomingMessage< IP::RequestType, IP::NotificationType > >;
    type Out = MessageEnvelope< OutgoingMessage< OP::RequestType, OP::NotificationType, OP::ResponseType > >;

    fn decode( &mut self, buf: &mut EasyBuf ) -> io::Result< Option< Self::In > > {
        loop {
            match self.codec_state {
                LSCodecState::Headers => {
                    if !self.read_header( buf )? {
                        return Ok( None );
                    }
                },
                LSCodecState::Body => {
                    return self.parse_body_contents( buf );
                }
            }
        }
    }

    fn encode( &mut self, msg : Self::Out, buffer : &mut Vec< u8 > ) -> io::Result< ( ) > {
        let MessageEnvelope{ mut headers, message } = msg;

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

impl < ReqType : RequestMessageImpl, NotifType : NotificationMessageImpl, ResType : serde::Serialize + fmt::Debug > OutgoingMessage< ReqType, NotifType, ResType > {

    /// Serializes this outgoing message as json.
    pub fn into_json( self ) -> String {
        match self {
            OutgoingMessage::Request( request ) => {
                let RequestMessage{ id, method } = request;

                let method_name = method.get_message_method( ).to_owned( );
                let params = serde_json::to_value( method );

                #[derive( Serialize )]
                struct RequestWrapper {
                    jsonrpc : &'static str,
                    id      : i64,
                    method  : String,
                    params  : serde_json::Value
                }

                let wrapper = RequestWrapper {
                    jsonrpc : "2.0",
                    id      : id,
                    method  : method_name,
                    params  : params
                };

                serde_json::to_string( &wrapper ).unwrap( )
            },
            OutgoingMessage::Notification( notif ) => {
                let NotificationMessage{ method } = notif;

                let method_name = method.get_message_method( ).to_owned( );
                let params = serde_json::to_value( method );

                #[derive( Serialize )]
                struct NotificationWrapper {
                    jsonrpc : &'static str,
                    method  : String,
                    params  : serde_json::Value
                }

                let wrapper = NotificationWrapper {
                    jsonrpc : "2.0",
                    method  : method_name,
                    params  : params
                };

                serde_json::to_string( &wrapper ).unwrap( )
            },
            OutgoingMessage::Response( response ) => {
                let ResponseMessage{ id, result, error } = response;

                let result = result.map( | result | serde_json::to_value( result ) );

                #[derive( Serialize )]
                struct ResponseWrapper {
                    jsonrpc : &'static str,
                    id      : i64,
                    #[serde( skip_serializing_if = "Option::is_none" )]
                    result  : Option< serde_json::Value >,
                    #[serde( skip_serializing_if = "Option::is_none" )]
                    error   : Option< ResponseError >
                }

                let wrapper = ResponseWrapper {
                    jsonrpc : "2.0",
                    id      : id,
                    result  : result,
                    error   : error
                };

                serde_json::to_string( &wrapper ).unwrap( )
            }
        }
    }

}