::fallbackGET <- function () {};

const PIx2 = 6.28318;
local pos = pplayer.eyes.GetOrigin();
local fvec = pplayer.eyes.GetForwardVector();

ppmod.detach(function (args):(pos, fvec) {

  while (args.ang < PIx2) {

    local vec = Vector(cos(args.ang), sin(args.ang)) * 2048;
    local frac = ppmod.ray(pos, pos + vec, "phys_bone_follower").fraction;

    args.points.push(vec * frac);

    args.ang += PIx2 / 96.0;

  }

  local json = "[[" + pos.x + "," + pos.y + "],["+ fvec.x +","+ fvec.y +"]";
  foreach (point in args.points) {
    json += ",[" + point.x + "," + point.y + "]";
  }
  json += "]";

  ::sendResponse(json, "application/json");

}, {
  ang = 0,
  points = []
});

