::fallbackGET <- function () {};

local pos = GetPlayer().GetOrigin();
::sendResponse(@"{ ""x"":"+ pos.x +@", ""y"":"+ pos.y +@", ""z"":"+ pos.z +@" }", "application/json");
