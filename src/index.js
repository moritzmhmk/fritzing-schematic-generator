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

customElements.define(
  "svg-xml-view",
  class extends HTMLElement {
    constructor() {
      super();
      this.xsltProcessor = new XSLTProcessor();
      const xsltDoc = new DOMParser().parseFromString(
        [
          // describes how we want to modify the XML - indent everything
          '<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform">',
          '  <xsl:strip-space elements="*"/>',
          '  <xsl:template match="para[content-style][not(text())]">', // change to just text() to strip space in text nodes
          '    <xsl:value-of select="normalize-space(.)"/>',
          "  </xsl:template>",
          '  <xsl:template match="node()|@*">',
          '    <xsl:copy><xsl:apply-templates select="node()|@*"/></xsl:copy>',
          "  </xsl:template>",
          '  <xsl:output indent="yes"/>',
          "</xsl:stylesheet>",
        ].join("\n"),
        "application/xml"
      );
      this.xsltProcessor.importStylesheet(xsltDoc);
      this.xmlSerializer = new XMLSerializer();
      this.observer = new MutationObserver(this.mutationCallback.bind(this));
    }
    connectedCallback() {
      this.setupObserver();
    }
    attributeChangedCallback() {
      this.setupObserver();
    }
    static get observedAttributes() {
      return ["svg-id"];
    }

    // Our function to set the textContent based on attributes.
    setTextContent() {
      const svgId = this.getAttribute("svg-id");
      this.textContent = svgId;
    }

    setupObserver() {
      this.observer.disconnect();
      this.targetNode = document.getElementById(this.getAttribute("svg-id"));
      this.observer.observe(this.targetNode, {
        attributes: true,
        characterData: true,
        childList: true,
        subtree: true,
      });
      this.mutationCallback()
    }

    mutationCallback() {
      const resultDoc = this.xsltProcessor.transformToDocument(this.targetNode);
      const resultString = this.xmlSerializer.serializeToString(resultDoc);
      this.textContent =
        '<?xml version="1.0" encoding="UTF-8" standalone="no"?>\n' +
        resultString;
    }
  }
);

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
