import "./main.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

var app = Elm.Main.init({
  flags: location.href,
  node: document.getElementById("root"),
});

// from https://github.com/elm/browser/blob/1.0.2/notes/navigation-in-elements.md

// Inform app of browser navigation (the BACK and FORWARD buttons)
// window.addEventListener('popstate', function () {
//    app.ports.onUrlChange.send(location.href);
//});

// Change the URL upon request, inform app of the change.
app.ports.pushUrl.subscribe(function (url) {
  history.pushState({}, "", url);
  //    app.ports.onUrlChange.send(location.href);
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
