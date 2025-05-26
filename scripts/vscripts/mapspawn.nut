if (!("Entities" in this)) return;
IncludeScript("ppmod4");

::sendResponse <- function (content, type = "text/html") {

  local lines = 0;
  for (local i = 0; i < content.len(); i ++) {
    if (content[i] == '\n') lines ++;
  }
  local contentLength = content.len() + lines;

  printl("Content-Type: "+ type +"; charset=utf-8");
  printl("Content-Length: " + contentLength);
  printl("");

  for (local i = 0; i < content.len(); i += 128) {
    local end = min(i + 128, content.len());
    print(content.slice(i, end));
  }

};

::handleGET <- function () {
  printl("HTTP/1.1 200 OK");

  ::fallbackGET <- function () {
    IncludeScript("pages/index");
  };

  // Listen for the presence of these headers for page indexing
  SendToConsole(@"alias P2-Coordinates ""script_execute pages/coordinates""");
  SendToConsole(@"alias P2-MapName ""script_execute pages/mapname""");
  SendToConsole(@"alias P2-TopDown ""script_execute pages/topdown""");
  SendToConsole(@"alias P2-Entities ""script_execute pages/entities""");
  SendToConsole(@"alias P2-Slice ""script_execute pages/slice""");

};

::handlePOST <- function () {
  printl("HTTP/1.1 200 OK");

  ::sendResponse("");

};

ppmod.onauto(async(function () {

  yield ppmod.player(GetPlayer());
  ::pplayer <- yielded;

  SendToConsole(@"alias Connection ""script fallbackGET()""");

  SendToConsole(@"alias GET ""script ::handleGET()""");
  SendToConsole(@"alias POST ""script ::handlePOST()""");

}));
