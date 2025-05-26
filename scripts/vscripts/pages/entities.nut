::fallbackGET <- function () {};

local ent = null, json = "{";

local classes = [
  "prop_weighted_cube",
  "prop_portal",
  "prop_testchamber_door"
];

for (local i = 0; i < classes.len(); i ++) {
  local entclass = classes[i];
  json += "\"" + entclass + "\":[";

  ent = ppmod.get(entclass, ent);
  if (ent) {
    local pos = ent.GetOrigin();
    json += "[" + pos.x + "," + pos.y + "]";
  }

  while (ent = ppmod.get(entclass, ent)) {
    local pos = ent.GetOrigin();
    json += ",[" + pos.x + "," + pos.y + "]";
  }

  json += "]";
  if (i != classes.len() - 1) json += ",";
}

json += "}";
::sendResponse(json, "application/json");
