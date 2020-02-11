open WebSockets;

let wsURI = "wss://echo.websocket.org/";
let websocket = WebSocket.make(wsURI);

{
  open WebSocket;
  let onClose = Close(
    (closeEvent, exitReason) => {
      Js.log(closeEvent);
      switch (exitReason) {
        | Normal => Js.log("Everything went smoothly")
        | _ => Js.log("Unexpected error")
      }
    });
  let onMessage = Message(
    (event) => {
      Js.log("The message is:");
      Js.log(event |> MessageEvent.stringData);
      }
  );
  let onError = Error(errorCode => Js.log(errorCode));

  websocket
  |> on(onClose)
  |> on(onMessage)
  |> on(onError);
}
