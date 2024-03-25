import "./style.css";
import { Elm } from "./src/Main.elm";

if (process.env.NODE_ENV === "development") {
  const ElmDebugTransform = await import("elm-debug-transformer");

  ElmDebugTransform.register({
    simple_mode: true,
  });
}

const root = document.querySelector("#app div");
Elm.Main.init({
  node: root,
  flags: {
    /**
     * update the value of the word. Optionally, remove the flag.
     * Designed with flags to take a value from a backend eventually
     * */
    word: "three",
  },
});
