open WebSockets;
let websocket = WebSocket.make("localhost:65000");

let closeEventHandle = (closeEvent, exitReason) => {
  open WebSocket.OnClose;
  Js.log(closeEvent);
  switch (exitReason) {
    | Normal => ()
    | _ => ()
    }
};

{
  open WebSocket;
  let event = Close(closeEventHandle);
  on(event, websocket);
}

Js.log(websocket);
