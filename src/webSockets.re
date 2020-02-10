module MessageEvent = {
  type t;
  [@bs.get] external data : t => 'a = "data";
  [@bs.get] external arrayBufferData : t => Js.Typed_array.array_buffer = "data";
  [@bs.get] external stringData : t => string = "data";
  [@bs.get] external origin : t => string = "origin";
  [@bs.get] external lastEventId : t => string = "lastEventId";
  [@bs.get] external source : t => Js.t({..}) = "source";
  [@bs.get] external ports : t => array (Js.t({..})) = "ports";
};

module CloseEvent = {
  type t;
  [@bs.get] external code : t => int = "code";
  [@bs.get] external reason : t => string = "reason";
  [@bs.get] external wasClean : t => bool = "wasClean";
};

module type WebSocketMaker = {
  type t;
  let make: string => t;
  let makeWithProtocols: (string, ~protocols :'a) => t;
};

module MakeWebSocket (Maker: WebSocketMaker) {
  type t = Maker.t;
  let make = Maker.make;
  let makeWithProtocol = (url, ~protocol: string) => Maker.makeWithProtocols(url, ~protocols=[protocol]);
  let makeWithProtocols = (url, ~protocols: list(string)) => Maker.makeWithProtocols(url, ~protocols=(Array.of_list(protocols)));
  type binaryType =
    | Blob
    | ArrayBuffer;
  [@bs.get] external _binaryType : t => string = "binaryType";
  [@bs.set] external _setBinaryType : t => string => unit = "binaryType";
  let binaryType = t => {
    let str = _binaryType(t);
    str == "blob" ? Blob : ArrayBuffer
  };
  let setBinaryType = (binaryType, t) => {
    let typestr =
      binaryType |> (
        fun
        | Blob => "blob"
        | ArrayBuffer => "arraybuffer"
      );
    _setBinaryType(t, typestr);
    t
  };
  [@bs.get] external bufferedAmount : t => int64 = "bufferedAmount";
  [@bs.get] external extensions : t => 'a = "extensions";
  module OnClose {
    type exitReason =
      | Normal // 1000
      | GoingAway // 1001
      | ProtocolError // 1002
      | UnsupportedData // 1003
      | NoStatus // 1005
      | AbnormalClosure // 1006
      | InvalidFrame // 1007
      | PolicyViolation // 1008
      | MessageTooLarge // 1009
      | MissingExtension // 1010
      | InternalError // 1011
      | ServiceRestart // 1012
      | TryAgainLater // 1013
      | BadGateway // 1014
      | TLSHandshakeFailure // 1015
      | UndefinedError // *
    ;
    let exitCodeToReason = exitCode =>
      switch (exitCode) {
        | 1000 => Normal
        | 1001 => GoingAway
        | 1002 => ProtocolError
        | 1003 => UnsupportedData
        | 1005 => NoStatus
        | 1006 => AbnormalClosure
        | 1007 => InvalidFrame
        | 1008 => PolicyViolation
        | 1009 => MessageTooLarge
        | 1010 => MissingExtension
        | 1011 => InternalError
        | 1012 => ServiceRestart
        | 1013 => TryAgainLater
        | 1014 => BadGateway
        | 1015 => TLSHandshakeFailure
        | _ => UndefinedError
      };
    type closingEvent = (CloseEvent.t, exitReason) => unit;
    // This enforces that the closing code is handled by our close connection
    // Can add more transformations to throw an exception.
    let closingTransform: closingEvent => (CloseEvent.t => unit) = closingFx => 
        (closeEvent) => closingFx(closeEvent, CloseEvent.code(closeEvent) |> exitCodeToReason);
  }
  type event =
    | Close (OnClose.closingEvent)
    | Error (string => unit)
    | Message (MessageEvent.t => unit)
    | Open (unit => unit);
  [@bs.send.pipe : t] external _on : string => (Js.t({..}) => unit) => unit = "addEventListener";
  let on = (e, t) => {
    let evtname =
      switch e {
      | Close(_) => "close"
      | Error(_) => "error"
      | Message(_) => "message"
      | Open(_) => "open"
      };
    _on(
      evtname,
      ((jsobj: Js.t({..})) =>
          switch e {
          | Close(fn) => OnClose.closingTransform(fn)(Obj.magic(jsobj))
          | Error(fn) => fn(jsobj##message)
          | Message(fn) => fn(Obj.magic(jsobj))
          | Open(fn) => fn()
          }
      ),
      t);
    t
  };
  [@bs.get] external protocol : t => string = "protocol";
  type readyState =
    | Connecting
    | Open
    | Closing
    | Closed;
  [@bs.get] external _readyState : t => int = "readyState";
  let readyState = (t) =>
    switch (_readyState(t)) {
    | 0 => Connecting
    | 1 => Open
    | 2 => Closing
    | _ => Closed
    };
  [@bs.get] external url : t => string = "url";
  [@bs.send.pipe : t] external close : unit => unit = "close";
  [@bs.send.pipe : t]external closeWithCode : int => unit = "close";
  [@bs.send.pipe : t]external closeWithReason : string => unit = "close";
  [@bs.send.pipe : t]external closeWithCodeAndReason : int => string => unit = "close";
  [@bs.send.pipe : t]external sendString : string => unit = "send";
  [@bs.send.pipe : t]external sendArrayBuffer : Js.Typed_array.array_buffer => unit = "send";
  type blob;
  [@bs.send.pipe : t] external sendBlob : blob => unit = "send";
};

module BrowserWebSocket = {
  type t;
  [@bs.new] external make : string => t = "WebSocket";
  [@bs.new] external makeWithProtocols: (string, ~protocols: 'a) => t = "WebSocket";
};

module WebSocket = MakeWebSocket(BrowserWebSocket);
/** Tips: If you need to make a WebSocket client on Nodejs, you can make a module to implement WebSocketMaker as BrowserWebSocket. e.g.
  *
  * WARNING: It's an untested example.
  *
 module WSWebSocket = {
   type t;
   external make : string => t = "WebSocket" [@@bs.new] [@@bs.module "ws"];
   external makeWithProtocols : string => protocols::'a => t =
     "WebSocket" [@@bs.new] [@@bs.module "ws"];
 };

 module NodeWSClient = MakeWebSocket WSWebSocket;
 *
 */
