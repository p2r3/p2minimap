::fallbackGET <- function () {};

local pos = pplayer.eyes.GetOrigin();

local fvec = pplayer.eyes.GetForwardVector().Normalize2D();
local uvec = Vector(0, 0, 1);

ppmod.detach(function (args):(pos, fvec, uvec) {

  while (args.ang < PI) {

    local vec = (fvec * sin(args.ang) + uvec * cos(args.ang)) * 1024.0;
    local point = ppmod.ray(pos, pos + vec, "phys_bone_follower").point;

    args.points.push([
      (pos - point).Length2D() / 1024.0,
      (point.z - pos.z) / 1024.0
    ]);

    args.ang += PI / 64.0;

  }

  local json = "[" + pplayer.eyes.GetAngles().x + ",";
  for (local i = 1; i < args.points.len(); i ++) {
    json += "[" + args.points[i][0] + "," + args.points[i][1] + "]";
    if (i != args.points.len() - 1) json += ",";
  }
  json += "]";

  ::sendResponse(json, "application/json");

}, {
  ang = 0,
  points = []
});

