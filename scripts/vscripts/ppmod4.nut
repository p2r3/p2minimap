/*
  ppmod version 4
  author: PortalRunner
*/

if (!("Entities" in this)) {
  printl("[ppmod] Error: ppmod4 was included in a scope without CEntities!");
  return;
}

if ("ppmod" in this) {
  printl("[ppmod] Error: ppmod is already loaded!");
  return;
}

::ppmod <- {};

/********************/
// Global Utilities //
/********************/

::min <- function (a, b) return a > b ? b : a;
::max <- function (a, b) return a < b ? b : a;
::round <- function (a, b = 0) return floor (a * (b = pow(10, b)) + 0.5) / b;

class pparray {

  constructor (size = 0, fill = null) {
    if (typeof size == "array") arr = size;
    else arr = array(size, fill);
  }

  function _typeof () return "array";
  function _get (idx) return arr[idx];
  function _set (idx, val) return arr[idx] = val;
  function _nexti (previdx) {
    if (this.len() == 0) return null;
    if (previdx == null) return 0;
    return previdx < this.len() - 1 ? previdx + 1 : null;
  }
  function _tostring () {
    local str = "[";
    for (local i = 0; i < arr.len(); i ++) {
      if (typeof arr[i] == "string") str += "\"" + arr[i] + "\"";
      else str += arr[i];
      if (i != arr.len() - 1) str += ", ";
    }
    return str + "]";
  }
  function _cmp (other) {
    for (local i = 0; i < min(arr.len(),other.len()); i ++) {
      if (arr[i] < other[i]) return -1;
      else if (arr[i] > other[i]) return 1;
    }
    if (arr.len() < other.len()) return -1;
    else if (arr.len() > other.len()) return 1;
    return 0;
  }

  function join (joinstr = "") {
    local str = "";
    for (local i = 0; i < arr.len(); i ++) {
      str += arr[i];
      if (i != arr.len() - 1) str += joinstr;
    }
    return str;
  }
  function len () return arr.len();
  function append (val) return arr.append(val);
  function push (val) return arr.push(val);
  function extend (other) return arr.extend(other);
  function pop () return arr.pop();
  function shift () return arr.remove(0);
  function unshift (val) return arr.insert(0, val);
  function top () return arr.top();
  function insert (idx, val) return arr.insert(idx, val);
  function remove (idx) return arr.remove(idx);
  function resize (size, fill = null) return arr.resize(size, fill);
  function sort (func = null) return func ? arr.sort(func) : arr.sort();
  function reverse () return arr.reverse();
  function slice (start, end = null) return pparray(arr.slice(start, end || arr.len()));
  function tostring () return _tostring();
  function clear () return arr.clear();
  function equals (other) {
    if (arr.len() != other.len()) return 0;
    for (local i = 0; i < arr.len(); i ++) {
      if (typeof arr[i] == "array"){
        if (arr[i].equals(other[i]) == 0) return 0;
      } else {
        if (arr[i] != other[i]) return 0;
      }
    }
    return 1;
  }
  function find (match) {
    if (typeof match == "function") {
      for (local i = 0; i < arr.len(); i ++) {
        if (match(arr[i])) return i;
      }
      return -1;
    }
    for (local i = 0; i < arr.len(); i ++) {
      if (arr[i] == match) return i;
    }
    return -1;
  }

  arr = null;

}

class ppheap {

  constructor (maxs = 0, comparator = null) {
    maxsize = maxs;
    arr = pparray(maxsize*4 + 1,0);
    if (comparator) {
      comp = comparator;
    } else {
      comp = function (a, b) { return a < b };
    }
  }

  function isempty () { return size == 0 };
  function bubbledown (hole) {
    local temp = arr[hole];
    while (hole * 2 <= size) {
      local child = hole * 2;
      if (child != size && comp(arr[child + 1], arr[child])) child ++;
      if (comp(arr[child], temp)) {
        arr[hole] = arr[child]
      } else {
        break;
      }
      hole = child;
    }
    arr[hole] = temp;
  }
  function remove () {
    if (isempty()) {
      throw "Heap is empty";
    } else {
      local tmp = arr[1];
      arr[1] = arr[size--];
      bubbledown(1);
      return tmp;
    }
  }
  function gettop () {
    if (isempty()) {
      throw "Heap is empty";
    } else {
      return arr[1];
    }
  }
  function insert (val) {
    if (size == maxsize) {
      throw "Exceeding max heap size";
    }
    arr[0] = val;
    local hole = ++size;
    while (comp(val, arr[hole / 2])) {
      arr[hole] = arr[hole / 2];
      hole /= 2;
    }
    arr[hole] = val;
  }

  arr = pparray([0]);
  size = 0;
  maxsize = 0;
  comp = null;

}

class ppstring {

  constructor (str = "") {
    string = str.tostring();
  }

  function _typeof () return "string";
  function _tostring () return string;
  function _add (other) return ppstring(string + other.tostring());
  function _get (idx) return string[idx];
  function _set (idx, val) return string = string.slice(0, idx) + val.tochar() + string.slice(idx + 1);
  function _cmp (other) {
    if (string == other.tostring()) return 0;
    if (string > other.tostring()) return 1;
    return -1;
  }

  function len () return string.len();
  function tointeger () return string.tointeger();
  function tofloat () return string.tofloat();
  function tostring () return string;
  function slice (start, end = null) return ppstring(string.slice(start, end || string.len()));
  function find (substr, startidx = 0) return string.find(substr, startidx);
  function tolower () return ppstring(string.tolower());
  function toupper () return ppstring(string.toupper());
  function split (substr) {
    local arr = [], curr = 0, prev = 0;
    while ((curr = string.find(substr, curr)) != null) {
      curr = max(curr, prev + 1);
      arr.push(string.slice(prev, curr));
      prev = curr += substr.len();
    }
    arr.push(string.slice(prev));
    return arr;
  }
  function strip () return ppstring(::strip(string));
  function lstrip () return ppstring(::lstrip(string));
  function rstrip () return ppstring(::rstrip(string));
  function replace (substr, rep) return ppstring(pparray(this.split(substr)).join(rep));

  string = null;

}

local ppromise_base = {

  then = function (onthen = null, oncatch = null) {

    if (typeof onthen != "function") onthen = identity;
    if (typeof oncatch != "function") oncatch = thrower;

    if (state == "fulfilled") { onthen(value); return this }
    if (state == "rejected") { oncatch(value); return this }

    onfulfill.push(onthen);
    onreject.push(oncatch);

    return this;

  },

  except = function (oncatch = null) {

    if (typeof oncatch != "function") oncatch = thrower;

    if (state == "rejected") return oncatch(value);
    onreject.push(oncatch);

    return this;

  },

  finally = function (onfinally) {

    if (state != "pending") return onfinally(value);
    onresolve.push(onfinally);

    return this;

  }

}

::ppromise <- function (func):(ppromise_base) {

  local inst = {

    onresolve = [],
    onfulfill = [],
    onreject = [],

    state = "pending",
    value = null,

    identity = function (x) { return x },
    thrower = function (x) { throw x },

    then = ppromise_base.then,
    except = ppromise_base.except,
    finally = ppromise_base.finally

    resolve = null,
    reject = null

  };

  inst.resolve = function (val = null):(inst) {

    if (inst.state != "pending") return;

    inst.state = "fulfilled";
    inst.value = val;

    for (local i = 0; i < inst.onfulfill.len(); i ++) inst.onfulfill[i](val);
    for (local i = 0; i < inst.onresolve.len(); i ++) inst.onresolve[i]();

  };

  inst.reject = function (err = null):(inst) {

    if (inst.state != "pending") return;

    inst.state = "rejected";
    inst.value = err;

    if (inst.onreject.len() == 0) inst.thrower(err);
    else for (local i = 0; i < inst.onreject.len(); i ++) inst.onreject[i](err);
    for (local i = 0; i < inst.onresolve.len(); i ++) inst.onresolve[i]();

  };

  try {
    func(inst.resolve, inst.reject);
  } catch (e) {
    inst.reject(e);
  }

  return inst;

}

::ppmod.asyncgen <- [];
::ppmod.asyncrun <- function (id, resolve, reject) {

  local next;
  try {
    next = resume ppmod.asyncgen[id];
  } catch (e) {
    return reject(e);
  }

  if (ppmod.asyncgen[id].getstatus() == "dead") {
    ppmod.asyncgen[id] = null;
    return resolve(next);
  }

  next.then(function (val):(id, resolve, reject) {
    ::yielded <- val;
    ppmod.asyncrun(id, resolve, reject);
  });

}

::yielded <- null;
::async <- function (func) {

  return function (...):(func) {

    local args = array(vargc + 1);
    for (local i = 0; i < vargc; i ++) args[i + 1] = vargv[i];
    args[0] = this;

    return ppromise(function (resolve, reject):(func, args) {

      for (local i = 0; i < ppmod.asyncgen.len(); i ++) {
        if (ppmod.asyncgen[i] == null) {
          ppmod.asyncgen[i] = func.acall(args);
          ppmod.asyncrun(i, resolve, reject);
          return;
        }
      }

      ppmod.asyncgen.push(func.acall(args));
      ppmod.asyncrun(ppmod.asyncgen.len() - 1, resolve, reject);

    });

  }

}

try {

  function Vector::_mul (other) {
    if (typeof other == "Vector") {
      return Vector(this.x * other.x, this.y * other.y, this.z * other.z);
    } else {
      return Vector(this.x * other, this.y * other, this.z * other);
    }
  }

  function Vector::_div (other) {
    if (typeof other == "Vector") {
      return Vector(this.x / other.x, this.y / other.y, this.z / other.z);
    } else {
      return Vector(this.x / other, this.y / other, this.z / other);
    }
  }

  function Vector::_unm () {
    return Vector() - this;
  }

  function Vector::equals (other) {
    if (this.x == other.x && this.y == other.y && this.z == other.z) return true;
    return false;
  }

  function Vector::_tostring () {
    return "Vector(" + this.x + ", " + this.y + ", " + this.z + ")";
  }

  function Vector::ToKVString () {
    return this.x + " " + this.y + " " + this.z;
  }

  function Vector::Normalize() {
    this.Norm();
    return this;
  }

  function Vector::Normalize2D() {
    this.z = 0.0;
    this.Norm();
    return this;
  }

} catch (e) {

  printl("[ppmod] Warning: failed to modify Vector class: " + e);

}

/*********************/
// Entity management //
/*********************/

::ppmod.get <- function (arg1, arg2 = null, arg3 = null, arg4 = null) {

  local curr = null;

  if (typeof arg1 == "string") {

    if (curr = Entities.FindByName(arg2, arg1)) return curr;
    if (curr = Entities.FindByClassname(arg2, arg1)) return curr;
    return Entities.FindByModel(arg2, arg1);

  }

  if (typeof arg1 == "Vector") {

    if (arg2 == null) arg2 = 32.0;

    if (typeof arg3 == "string") {
      local filter = arg3;
      arg3 = arg4;
      arg4 = filter;
    }

    if (typeof arg4 == "string") {

      while (arg3 = Entities.FindInSphere(arg3, arg1, arg2)) {
        if (!arg3.IsValid()) continue;
        if (arg3.GetName() == arg4 || arg3.GetClassname() == arg4 || arg3.GetModelName() == arg4) {
          return arg3;
        }
      }

      return null;

    } else {
      return Entities.FindInSphere(arg3, arg1, arg2);
    }

  }

  if (typeof arg1 == "integer") {

    while (curr = Entities.Next(curr)) {
      if (!curr.IsValid()) continue;
      if (curr.entindex() == arg1) return curr;
    }

    return null;

  }

  if (typeof arg1 == "instance" && arg1 instanceof CBaseEntity) {
    return ent;
  }
  return null;

}

::ppmod.getall <- function (args, callback) {

  if (typeof args != "array") {
    args = [args];
  }
  args.push(null);
  args.insert(0, this);

  local curr = null;

  while (true) {

    curr = ppmod.get.acall(args);

    if (!curr) return;
    callback(curr);

    args[args.len() - 1] = curr;

  }

}

::ppmod.prev <- function (...) {

  local start = null, curr = null, prev = null;

  if (vargc > 1) {
    start = vargv[vargc - 1];
    curr = start;
  }

  while (true) {

    if (vargc < 3) curr = ppmod.get(vargv[0], curr);
    else if (vargc == 3) curr = ppmod.get(vargv[0], vargv[1], curr);
    else curr = ppmod.get(vargv[0], vargv[1], vargv[2], curr);

    if (curr == start) return prev;
    prev = curr;

  }

}

::ppmod.fire <- function (ent, action = "Use", value = "", delay = 0.0, activator = null, caller = null) {

  if (typeof ent == "string") {
    DoEntFire(ent, action, value.tostring(), delay, activator, caller);
    return;
  }

  if (!(typeof ent == "instance" && ent instanceof CBaseEntity)) {
    ppmod.getall(ent, function (curr):(action, value, delay, activator, caller) {
      ppmod.fire(curr, action, value, delay, activator, caller);
    });
    return;
  }

  EntFireByHandle(ent, action, value.tostring(), delay, activator, caller);

}

::ppmod.keyval <- function (ent, key, val) {

  if (!(typeof ent == "instance" && ent instanceof CBaseEntity)) {
    ppmod.getall(ent, function (curr):(key, val) {
      ppmod.keyval(curr, key, val);
    });
    return;
  }

  switch (typeof val) {

    case "integer":
    case "bool":
      ent.__KeyValueFromInt(key, val.tointeger());
      break;
    case "float":
      ent.__KeyValueFromFloat(key, val);
      break;
    case "Vector":
      ent.__KeyValueFromVector(key, val);
      break;
    default:
      ent.__KeyValueFromString(key, val.tostring());

  }

}

::ppmod.flags <- function (ent, ...) {

  local sum = 0;
  for (local i = 0; i < vargc; i ++) {
    sum += vargv[i];
  }

  ppmod.keyval(ent, "SpawnFlags", sum);

}

::ppmod.addoutput <- function (ent, output, target, input = "Use", value = "", delay = 0, max = -1) {

  if (typeof target != "string") {

    ppmod.addscript(ent, output, function ():(target, input, value) {
      ppmod.fire(target, input, value, 0.0, activator, caller);
    }, delay, max, false);

    return;

  }

  ppmod.keyval(ent, output, target+"\x1B"+input+"\x1B"+value+"\x1B"+delay+"\x1B"+max);

}

::ppmod.scrq <- [];

::ppmod.scrq_add <- function (scr, max = -1) {

  if (typeof scr == "string") {
    scr = compilestring(scr);
  }

  for (local i = 0; i < ppmod.scrq.len(); i ++) {
    if (!ppmod.scrq[i]) {
      ppmod.scrq[i] = [scr, max];
      return i;
    }
  }

  ppmod.scrq.push([scr, max]);
  return ppmod.scrq.len() - 1;

}

::ppmod.scrq_get <- function (idx) {

  local scr = ppmod.scrq[idx][0];

  if (ppmod.scrq[idx][1] > 0 && --ppmod.scrq[idx][1] == 0) {
    ppmod.scrq[idx] = null;
  }

  return scr;

}

::ppmod.addscript <- function (ent, output, scr = "", delay = 0, max = -1, passthrough = false) {

  if (typeof scr == "function") {
    if (passthrough) scr = "ppmod.scrq_get(" + ppmod.scrq_add(scr, max) + ")(activator, caller)";
    else scr = "ppmod.scrq_get(" + ppmod.scrq_add(scr, max) + ")()";
  }

  ppmod.keyval(ent, output, "worldspawn\x001BRunScriptCode\x1B"+scr+"\x1B"+delay+"\x1B"+max);

}

::ppmod.runscript <- function (ent, scr) {

  if (typeof scr == "function") {
    scr = "ppmod.scrq_get(" + ppmod.scrq_add(scr, 1) + ")()";
  }

  ppmod.fire(ent, "RunScriptCode", scr);

}

::ppmod.setparent <- function (child, _parent) {

  if (_parent) ppmod.fire(child, "SetParent", "!activator", 0, _parent);
  else ppmod.fire(child, "ClearParent");

}

::ppmod.hook <- function (ent, input, scr, max = -1) {

  if (!(typeof ent == "instance" && ent instanceof CBaseEntity)) {
    ppmod.getall(ent, function (curr):(input, scr, max) {
      ppmod.hook(curr, input, scr, max);
    });
    return;
  }

  if (!ent.ValidateScriptScope()) {
    throw "ppmod.hook: Could not validate entity script scope";
  }

  if (scr == null) ent.GetScriptScope()["Input"+input] <- function () return true;
  else ent.GetScriptScope()["Input"+input] <- ppmod.scrq_get(ppmod.scrq_add(scr, max));

}

// Implement shorthands of the above functions into the entities as methods
local entclasses = [CBaseEntity, CBaseAnimating, CBaseFlex, CBasePlayer, CEnvEntityMaker, CLinkedPortalDoor, CPortal_Player, CPropLinkedPortalDoor, CSceneEntity, CTriggerCamera];
for (local i = 0; i < entclasses.len(); i ++) {

  try {

    entclasses[i]._set <- function (key, val) {
      switch (typeof val) {
        case "integer":
        case "bool":
          this.__KeyValueFromInt(key, val.tointeger());
          break;
        case "float":
          this.__KeyValueFromFloat(key, val);
          break;
        case "Vector":
          this.__KeyValueFromVector(key, val);
          break;
        default:
          this.__KeyValueFromString(key, val.tostring());
      }
      return val;
    }
    entclasses[i]._get <- function (key) {
      return function (value = "", delay = 0.0, activator = null, caller = null):(key) {
        return ::EntFireByHandle(this, key, value.tostring(), delay, activator, caller);
      }
    }

    entclasses[i].Fire <- function (action = "Use", value = "", delay = 0.0, activator = null, caller = null) {
      return ::EntFireByHandle(this, action, value.tostring(), delay, activator, caller);
    }
    entclasses[i].AddOutput <- function (output, target, input = "Use", value = "", delay = 0, max = -1) {
      return ::ppmod.addoutput(this, output, target, input, value, delay, max);
    }
    entclasses[i].AddScript <- function (output, scr = "", delay = 0, max = -1, passthrough = false) {
      return ::ppmod.addscript(this, output, scr, delay, max, passthrough);
    }
    entclasses[i].RunScript <- function (scr) {
      return ::ppmod.runscript(this, scr);
    }
    entclasses[i].SetMoveParent <- function (_parent) {
      return ::ppmod.setparent(this, _parent);
    }
    entclasses[i].SetHook <- function (input, scr, max = -1) {
      return ::ppmod.hook(this, input, scr, max);
    }

  } catch (e) {

    local classname;
    switch (entclasses[i]) {
      case CBaseEntity: classname = "CBaseEntity"; break;
      case CBaseAnimating: classname = "CBaseAnimating"; break;
      case CBaseFlex: classname = "CBaseFlex"; break;
      case CBasePlayer: classname = "CBasePlayer"; break;
      case CEnvEntityMaker: classname = "CEnvEntityMaker"; break;
      case CLinkedPortalDoor: classname = "CLinkedPortalDoor"; break;
      case CPortal_Player: classname = "CPortal_Player"; break;
      case CPropLinkedPortalDoor: classname = "CPropLinkedPortalDoor"; break;
      case CSceneEntity: classname = "CSceneEntity"; break;
      case CTriggerCamera: classname = "CTriggerCamera"; break;
    }

    printl("[ppmod] Warning: failed to modify " + classname + " class: " + e);

  }

}

/****************/
// Control flow //
/****************/

::ppmod.wait <- function (scr, sec, name = "") {

  local relay = Entities.CreateByClassname("logic_relay");
  relay.__KeyValueFromString("Targetname", name);

  ppmod.addscript(relay, "OnTrigger", scr, 0, 1);
  EntFireByHandle(relay, "Trigger", "", sec, null, null);
  relay.__KeyValueFromInt("SpawnFlags", 1);

  return relay;

}

::ppmod.interval <- function (scr, sec = 0.0, name = "") {

  local timer = Entities.CreateByClassname("logic_timer");
  timer.__KeyValueFromString("Targetname", name);

  ppmod.addscript(timer, "OnTimer", scr);
  EntFireByHandle(timer, "RefireTime", sec.tostring(), 0.0, null, null);
  EntFireByHandle(timer, "Enable", "", 0.0, null, null);

  return timer;

}

::ppmod.ontick <- function (scr, pause = true, timeout = -1) {

  if (typeof scr == "function") {
    if (timeout == -1) scr = "ppmod.scrq_get(" + ppmod.scrq_add(scr, -1) + ")()";
    else scr = "ppmod.scrq_get(" + ppmod.scrq_add(scr, 1) + ")()";
  }

  if (pause && FrameTime() == 0.0) {
    SendToConsole("script ppmod.ontick(\"" + scr + "\", true, " + timeout + ")");
    return;
  }

  if (timeout == -1) {
    SendToConsole("script " + scr + ";script ppmod.ontick(\"" + scr + "\", " + pause + ")");
    return;
  }

  if (timeout == 0) SendToConsole("script " + scr);
  else SendToConsole("script ppmod.ontick(\"" + scr + "\", " + pause + ", " + (timeout - 1) + ")");

}

::ppmod.once <- function (scr, name = null) {

  if (!name) name = scr.tostring();
  if (Entities.FindByName(null, name)) return;

  local relay = Entities.CreateByClassname("logic_relay");
  relay.__KeyValueFromString("Targetname", name);

  ppmod.addscript(relay, "OnTrigger", scr, 0, 1);
  EntFireByHandle(relay, "Trigger", "", 0.0, null, null);

  return relay;

}

::ppmod.onauto <- function (scr, onload = false) {

  local auto = Entities.CreateByClassname("logic_auto");

  // In online multiplayer games, we delay spawning until both players are ready
  if (IsMultiplayer()) scr = function ():(scr) {

    local outerinterval = UniqueString("ppmod_auto_outerinterval");

    ppmod.interval(function ():(scr, outerinterval) {

      // Find the host player, typically the first player named "blue"
      local blue = Entities.FindByName(null, "blue");
      if (!blue || !blue.IsValid() || blue.GetClassname() != "player") {
        blue = Entities.FindByClassname(null, "player");
      }
      if (!blue || !blue.IsValid()) return;

      Entities.FindByName(null, outerinterval).Destroy();

      if (IsLocalSplitScreen()) {
        if (typeof scr == "string") compilestring(scr)();
        else scr();
        return;
      }

      // Find the lowest significant point of the world's bounding box estimate
      local ent = null, lowest = 0, curr;
      while (ent = Entities.Next(ent)) {

        if (!ent.IsValid()) continue;

        curr = ent.GetOrigin().z + ent.GetBoundingMins().z;
        if (curr < lowest) lowest = curr;

      }

      // Additional decrement just to make sure we're below anything significant
      lowest -= 1024.0;

      // We move the host below the map and wait until they are teleported back up
      // This happens once both players finish connecting in network games
      blue.SetOrigin(Vector(0, 0, lowest));

      local intervalname = UniqueString("ppmod_auto_interval");
      ppmod.interval(function ():(blue, lowest, scr, intervalname) {

        local red = Entities.FindByClassname(null, "red");
        if (!red || !red.IsValid() || red.GetClassname() != "player") {
          red = Entities.FindByClassname(blue, "player");
        }

        if (!red || !red.IsValid() || blue.GetOrigin().z <= lowest) return;

        if (typeof scr == "string") compilestring(scr)();
        else scr();

        Entities.FindByName(null, intervalname).Destroy();

      }, 0.0, intervalname);

    }, 0.0, outerinterval);

  };

  ppmod.addscript(auto, "OnNewGame", scr);
  ppmod.addscript(auto, "OnMapTransition", scr);
  if (onload) ppmod.addscript(auto, "OnLoadGame", scr);

  return auto;

}

::ppmod.detach <- function (scr, args, stack = null) {

  if (stack == null) stack = getstackinfos(2);

  try {

    scr(args);

  } catch (e) {

    if (e == "Script terminated by SQQuerySuspend") {
      ppmod.detach(scr, args, stack);
      return;
    }

    printl("\nAN ERROR HAS OCCURED [" + e + "]");
    printl("Caught within ppmod.detach in file " + stack.src + " on line " + stack.line + "\n");

  }

}

/********************/
// Player interface //
/********************/

::ppmod.player <- function (player) {

  local pplayer = {};
  pplayer.ent <- player;

  // One logic_playerproxy is required for registering jumping and ducking
  local proxy = Entities.FindByClassname(null, "logic_playerproxy");
  if (!proxy) proxy = Entities.CreateByClassname("logic_playerproxy");

  // Set up a logic_measure_movement for more accurate view angles
  pplayer.eyes <- Entities.CreateByClassname("logic_measure_movement");
  local eyename = UniqueString("ppmod_eyes");

  pplayer.eyes.__KeyValueFromInt("MeasureType", 1);
  pplayer.eyes.__KeyValueFromString("Targetname", eyename);
  pplayer.eyes.__KeyValueFromString("TargetReference", eyename);
  pplayer.eyes.__KeyValueFromString("Target", eyename);
  pplayer.eyes.SetAngles(0, 0, 90.0);

  EntFireByHandle(pplayer.eyes, "SetMeasureReference", eyename, 0.0, null, null);
  EntFireByHandle(pplayer.eyes, "Enable", "", 0.0, null, null);

  // logic_measure_movement relies on targetname for selecting entities
  // This changes the player's targetname briefly and set it back right away
  local nameswap = function ():(pplayer) {

    local playername = pplayer.ent.GetName();
    pplayer.ent.__KeyValueFromString("Targetname", UniqueString("pplayer"));

    EntFireByHandle(pplayer.eyes, "SetMeasureTarget", pplayer.ent.GetName(), 0.0, null, null);

    ppmod.wait(function ():(pplayer, playername) {
      pplayer.ent.__KeyValueFromString("Targetname", playername);
    }, FrameTime());

  };

  nameswap(); // Swap the player's name now, and on every next load
  local auto = Entities.CreateByClassname("logic_auto");
  auto.__KeyValueFromString("OnMapSpawn", "!self\x001BRunScriptCode\x001Bppmod.scrq_get(" + ppmod.scrq_add(nameswap, -1) + ")()\x001B0\x001B-1");

  // Set up a game_ui for listening to player movement inputs
  local gameui = Entities.CreateByClassname("game_ui");
  gameui.__KeyValueFromInt("FieldOfView", -1);
  EntFireByHandle(gameui, "Activate", "", 0.0, player, null);

  // Used later for attaching outputs to the player landing after a fall
  local landrl = Entities.CreateByClassname("logic_relay");

  // Some unique values are saved in the player's script scope
  if (player.ValidateScriptScope()) {

    // Set up a simple loop for watching if the player is grounded
    local vel = player.GetVelocity().z;

    player.GetScriptScope()["ppmod_player_prevZ"] <- vel;
    player.GetScriptScope()["ppmod_player_grounded"] <- vel == 0;

    ppmod.interval(function ():(player, landrl) {

      local vel = player.GetVelocity().z;

      local prev = player.GetScriptScope()["ppmod_player_prevZ"];
      local grounded = player.GetScriptScope()["ppmod_player_grounded"];

      if (prev != 0 && vel != 0) grounded = false;
      if (prev <= 0 && vel == 0 && !grounded) {
        grounded = true;
        EntFireByHandle(landrl, "Trigger", "", 0.0, null, null);
      }

      player.GetScriptScope()["ppmod_player_prevZ"] <- vel;
      player.GetScriptScope()["ppmod_player_grounded"] <- grounded;

    });

    // Set up a trigger_gravity for modifying the player's local gravity
    ppmod.trigger(player.GetOrigin() + Vector(0, 0, 36.5), Vector(16, 16, 36), "trigger_gravity", Vector(), true).then(function (trigger):(player) {

      trigger.__KeyValueFromFloat("Gravity", 1.0);
      EntFireByHandle(trigger, "Disable", "", 0.0, null, null);
      player.GetScriptScope()["ppmod_player_gravity"] <- trigger;

      ppmod.interval(function ():(trigger, player) {
        trigger.SetAbsOrigin(player.GetCenter());
      });

    });

  }

  pplayer.holding <- function (classes = null):(player) {

    // List of all known holdable entities
    if (classes == null) {
      classes = pparray([
        "prop_weighted_cube",
        "prop_monster_box",
        "prop_physics",
        "prop_physics_override",
        "prop_physics_paintable",
        "npc_personality_core",
        "npc_portal_turret_floor",
        "npc_security_camera",
        "prop_glass_futbol"
      ]);
    } else if (!(classes instanceof pparray)) {
      classes = pparray(classes);
    }

    return ppromise(function (resolve, reject):(classes) {

      local scrqid = ppmod.scrq_add(resolve, 1);
      local name = UniqueString("ppmod_holding");

      local filter = Entities.CreateByClassname("filter_player_held");
      filter.__KeyValueFromString("Targetname", name);
      filter.__KeyValueFromString("OnPass", "!self\x001BRunScriptCode\x001Bppmod.scrq_get(" + scrqid + ")(true);self.Destroy()\x001B0\x001B1");

      local relay = Entities.CreateByClassname("logic_relay");
      relay.__KeyValueFromString("OnUser1", name + "\x001BRunScriptCode\x001Bppmod.scrq_get(" + scrqid + ")(false);self.Destroy()\x001B0\x001B1");
      relay.__KeyValueFromString("OnUser1", "!self\x001BOnUser2\x1B\x001B0\x001B1");
      relay.__KeyValueFromString("OnUser2", "!self\x001BKill\x1B\x001B0\x001B1");

      // Due to filter_player_held not differentiating between players,
      // Co-op checks only a 128u radius from the player, assuming VM grab controller
      if (IsMultiplayer()) {

        local curr = null;
        while (curr = Entities.FindInSphere(curr, player.GetCenter(), 128.0)) {

          if (!curr.IsValid()) continue;
          if (classes.find(curr.GetClassname()) == -1) continue;

          EntFireByHandle(filter, "TestActivator", "", 0.0, curr, null);

        }

      } else {

        local curr = null;
        while (curr = Entities.Next(curr)) {

          if (!curr.IsValid()) continue;
          if (classes.find(curr.GetClassname()) == -1) continue;

          EntFireByHandle(filter, "TestActivator", "", 0.0, curr, null);

        }

      }

      EntFireByHandle(relay, "FireUser1", "", 0.0, null, null);
      EntFireByHandle(relay, "Kill", "", 0.0, null, null);

    });

  };

  pplayer.jump <- function (scr):(player, proxy) {
    local scrqstr = "ppmod.scrq_get(" + ppmod.scrq_add(scr) + ")()";
    ppmod.addoutput(proxy, "OnJump", player, "RunScriptCode", "if (self == activator) " + scrqstr);
  };

  pplayer.land <- function (scr):(landrl) {
    ppmod.addscript(landrl, "OnTrigger", scr);
  };
  pplayer.grounded <- function ():(player) {
    return player.GetScriptScope()["ppmod_player_grounded"];
  };

  pplayer.duck <- function (scr):(player, proxy) {
    local scrqstr = "ppmod.scrq_get(" + ppmod.scrq_add(scr) + ")()";
    ppmod.addoutput(proxy, "OnDuck", player, "RunScriptCode", "if (self == activator) " + scrqstr);
  };

  pplayer.unduck <- function (scr):(player, proxy) {
    local scrqstr = "ppmod.scrq_get(" + ppmod.scrq_add(scr) + ")()";
    ppmod.addoutput(proxy, "OnUnDuck", player, "RunScriptCode", "if (self == activator) " + scrqstr);
  };

  pplayer.ducking <- function ():(player) {
    return player.GetCenter().z - player.GetOrigin().z < 18.001;
  };

  pplayer.input <- function (str, scr):(gameui) {
    if (str[0] == '+') str = "pressed" + str.slice(1);
    else str = "unpressed" + str.slice(1);
    ppmod.addscript(gameui, str, scr);
  };

  pplayer.gravity <- function (gravity):(player) {
    local trigger = player.GetScriptScope()["ppmod_player_gravity"];
    if (gravity == 1.0) EntFireByHandle(trigger, "Disable", "", 0.0, null, null);
    else EntFireByHandle(trigger, "Enable", "", 0.0, null, null);
    // Zero values have no effect, this is hacky but works well enough
    if (gravity == 0.0) trigger.__KeyValueFromString("Gravity", "0.0000000000000001");
    else trigger.__KeyValueFromFloat("Gravity", gravity);
  };

  pplayer.friction <- function (fric, ftime = null, grounded = null):(pplayer) {

    // Don't touch velocity if the player isn't grounded
    if (grounded == false) return;
    if (grounded == null && !pplayer.grounded()) return;

    if (ftime == null) ftime = FrameTime();

    local vel = pplayer.ent.GetVelocity();
    local veldir = vel + Vector();
    local absvel = veldir.Norm();

    // Cancel out existing friction calculations
    if (absvel >= 100) {
      vel *= 1 / (1 - ftime * 4);
    } else {
      vel += veldir * (ftime * 400);
    }

    // Simulate our own friction
    absvel = vel.Length();

    if (absvel >= 100) {
      vel *= 1 - ftime * fric;
    } else if (fric > 0) {
      if (fric / 0.6 < absvel) {
        vel -= veldir * (ftime * 400);
      } else if (absvel > 0) {
        vel *= mask;
      }
    }

    // Apply calculated velocity
    pplayer.ent.SetVelocity(vel);

  };

  pplayer.movesim <- function (move, accel = 10, fric = 0, sfric = 0.25, grav = null, ftime = null, eyes = null):(player, pplayer) {

    if (ftime == null) ftime = FrameTime();
    if (eyes == null) eyes = pplayer.eyes;
    if (grav == null) grav = Vector(0, 0, -600);

    if (!pplayer.grounded()) accel *= sfric;

    local vel = player.GetVelocity();
    local mask = Vector(0, 0, 1);

    if (fric > 0) {

      local veldir = vel + Vector();
      local absvel = veldir.Norm();

      if (absvel >= 100) {
        vel *= 1 - ftime * fric;
      } else if (fric / 0.6 < absvel) {
        vel -= veldir * (ftime * 400);
      } else if (absvel > 0) {
        vel *= mask;
      }

    }

    local forward = eyes.GetForwardVector();
    local left = eyes.GetLeftVector();
    forward -= forward * mask;
    left -= left * mask;

    forward.Norm();
    left.Norm();

    local wishvel = Vector();
    wishvel.x = forward.x * move.y + left.x * move.x;
    wishvel.y = forward.y * move.y + left.y * move.x;
    wishvel.z = forward.z * move.y + left.z * move.x;
    wishvel -= wishvel * mask;
    local wishspeed = wishvel.Norm();

    local horizvel = vel - vel * mask;
    local currspeed = horizvel.Dot(wishvel);

    local addspeed = wishspeed - currspeed;
    local accelspeed = accel * ftime * wishspeed;
    if (accelspeed > addspeed) accelspeed = addspeed;

    local finalvel = vel + wishvel * accelspeed + grav * ftime;
    player.SetVelocity(finalvel);

  };

  // Resolve the ppromise once pplayer.eyes returns a valid roll angle value and a trigger_gravity has been created
  // These are typically the lengthiest operations, hence why we're checking for these in particular
  return ppromise(function (resolve, reject):(pplayer) {

    local intervalname = UniqueString("player_enable");
    ppmod.interval(function ():(resolve, pplayer, intervalname) {

      if (pplayer.eyes.GetAngles().z != 90.0 && ("ppmod_player_gravity" in pplayer.ent.GetScriptScope())) {
        resolve(pplayer);
        Entities.FindByName(null, intervalname).Destroy();
      }

    }, 0, intervalname);

  });

}

::ppmod.portal <- function (portal) {

  if (!portal.ValidateScriptScope()) {
    throw "[ppmod] Error: Could not validate entity script scope in ppmod.portal";
  }

  local scope = portal.GetScriptScope();
  if ("ppmod_portal" in scope) return scope.ppmod_portal;

  local trigger = Entities.CreateByClassname("trigger_multiple");

  trigger.__KeyValueFromInt("Solid", 3);
  trigger.SetAbsOrigin(portal.GetOrigin());
  trigger.SetForwardVector(portal.GetForwardVector());
  trigger.SetSize(Vector(-8, -32, -56), Vector(0, 32, 56));

  trigger.__KeyValueFromInt("CollisionGroup", 10);
  trigger.__KeyValueFromInt("SpawnFlags", 11);
  EntFireByHandle(trigger, "Enable", "", 0.0, null, null);

  local scrq_idx = ppmod.scrq_add(function (ent):(scope) {
    ppmod.runscript("worldspawn", function ():(ent, scope) {

      local ticks_now = (Time() / FrameTime()).tointeger();
      local ticks_tp = (scope.ppmod_portal.tptime / FrameTime()).tointeger();

      // 1 tick tolerance, ideally 0 one day
      if (ticks_now - ticks_tp > 1) return;

      for (local i = 0; i < scope.ppmod_portal.tpfunc.len(); i ++) {
        scope.ppmod_portal.tpfunc[i](ent);
      }

    });
  }, -1);

  trigger.__KeyValueFromString("OnEndTouch", "worldspawn\x001BRunScriptCode\x001Bppmod.scrq_get(" + scrq_idx + ")(activator)\x001B0\x001B-1");
  portal.__KeyValueFromString("OnEntityTeleportFromMe", "!self\x001BRunScriptCode\x001Bself.GetScriptScope().ppmod_portal.tptime<-Time()\x001B0\x001B-1");

  EntFireByHandle(trigger, "SetParent", "!activator", 0.0, portal, null);

  local OnTeleport = function (func):(scope) {
    scope.ppmod_portal.tpfunc.push(func);
  };

  local new_detector = function (allids):(portal) {

    local detector = Entities.CreateByClassname("func_portal_detector");

    detector.__KeyValueFromInt("Solid", 3);
    detector.SetAbsOrigin(portal.GetOrigin());
    detector.SetSize(Vector(-0.1, -0.1, -0.1), Vector(0.1, 0.1, 0.1));

    detector.__KeyValueFromInt("CollisionGroup", 10);
    detector.__KeyValueFromInt("CheckAllIDs", allids);

    EntFireByHandle(detector, "Enable", "", 0.0, null, null);

    return detector;

  };

  local GetColor = function ():(new_detector) {
    return ppromise(function (resolve, reject):(new_detector) {

      local scrq_idx = ppmod.scrq_add(resolve, 1);

      local detector = new_detector(1);
      detector.__KeyValueFromString("OnStartTouchPortal1", "!self\x001BRunScriptCode\x001Bppmod.scrq_get(" + scrq_idx + ")(1);self.Destroy()\x001B0\x001B1");
      detector.__KeyValueFromString("OnStartTouchPortal2", "!self\x001BRunScriptCode\x001Bppmod.scrq_get(" + scrq_idx + ")(2);self.Destroy()\x001B0\x001B1");

    });
  };

  local GetActivatedState = function ():(new_detector) {
    return ppromise(function (resolve, reject):(new_detector) {

      local scrq_idx = ppmod.scrq_add(resolve, 1);

      local detector = new_detector(1);
      detector.__KeyValueFromString("OnStartTouchLinkedPortal", "!self\x001BRunScriptCode\x001Bppmod.scrq_get(" + scrq_idx + ")(true);self.Destroy()\x001B0\x001B1");
      detector.__KeyValueFromString("OnUser1", "!self\x001BRunScriptCode\x001Bif(self.IsValid())ppmod.scrq_get(" + scrq_idx + ")(false)\x001B0\x001B1");
      detector.__KeyValueFromString("OnUser1", "!self\x001BKill\x001B\x001B0\x001B1");
      EntFireByHandle(detector, "FireUser1", "", 0.0, null, null);

    });
  };

  local GetLinkageGroupID = function ():(new_detector, portal) {
    return ppromise(function (resolve, reject):(new_detector, portal) {

      local detector = new_detector(0);
      local params = { id = 0 };

      local check = function ():(detector, params, portal) {

        if (!detector.IsValid()) return;
        params.id ++;

        detector.__KeyValueFromInt("LinkageGroupID", params.id);
        detector.SetAbsOrigin(portal.GetOrigin());
        EntFireByHandle(detector, "Enable", "", 0.0, null, null);

        EntFireByHandle(detector, "FireUser1", "", 0.0, null, null);

      };

      local scrq_idx_resolve = ppmod.scrq_add(resolve, 1);
      local scrq_idx_params = ppmod.scrq_add(params, 1);
      local scrq_idx_check = ppmod.scrq_add(check, -1);

      detector.__KeyValueFromString("OnStartTouchPortal", "!self\x001BRunScriptCode\x001Bppmod.scrq_get(" + scrq_idx_resolve + ")(ppmod.scrq_get(" + scrq_idx_params + ").id);ppmod.scrq[" + scrq_idx_check + "] = null;self.Destroy()\x001B0\x001B1");
      detector.__KeyValueFromString("OnUser1", "!self\x001BRunScriptCode\x001Bif(self.IsValid())ppmod.scrq_get(" + scrq_idx_check + ")()\x001B0\x001B-1");

      EntFireByHandle(detector, "FireUser1", "", 0.0, null, null);

    });
  };

  local GetPartnerInstance = function ():(portal, GetLinkageGroupID) {
    return ppromise(function (resolve, reject):(portal, GetLinkageGroupID) {

      GetLinkageGroupID().then(function (id):(resolve, portal) {

        local param = {};
        param.next <- function (curr):(id, resolve, portal, param) {

          curr = Entities.FindByClassname(curr, "prop_portal");
          if (curr == null) return resolve(null);

          if (curr == portal) return param.next(curr);
          local pportal = ppmod.portal(curr);

          pportal.GetLinkageGroupID().then(function (currid):(resolve, param, curr, pportal, id) {

            if (currid != id) return param.next(curr);

            pportal.GetActivatedState().then(function (state):(resolve, param, curr) {
              if (state) return resolve(curr);
              return param.next(curr);
            });

          });

        };

        param.next(null);

      });

    });
  };

  scope.ppmod_portal <- {
    tptime = 0.0,
    tpfunc = [],
    OnTeleport = OnTeleport,
    GetColor = GetColor,
    GetActivatedState = GetActivatedState,
    GetLinkageGroupID = GetLinkageGroupID,
    GetPartnerInstance = GetPartnerInstance
  };

  return scope.ppmod_portal;

}

::ppmod.onportal_func <- [];
::ppmod.onportal <- function (func) {

  ppmod.onportal_func.push(func);
  if (ppmod.onportal_func.len() != 1) return;

  ::ppmod.onportal_check <- function (portal, first) {

    ppmod.runscript("worldspawn", function ():(portal, first) {

      local pgun = null;
      local color = null;

      while (pgun = Entities.FindByClassname(pgun, "weapon_portalgun")) {

        local scope = pgun.GetScriptScope();

        if (scope["ppmod_onportal_attack_time"] == Time()) {
          color = 1;
          break;
        }

        if (scope["ppmod_onportal_attack2_time"] == Time()) {
          color = 2;
          break;
        }

      }

      if (color == null) pgun = null;

      for (local i = 0; i < ppmod.onportal_func.len(); i ++) {
        ppmod.onportal_func[i]({
          portal = portal,
          weapon = pgun,
          color = color,
          first = first
        });
      }

    });

  };

  ppmod.interval(function () {

    local pgun = null;
    while (pgun = Entities.FindByClassname(pgun, "weapon_portalgun")) {

      if (!pgun.IsValid()) continue;
      if (!pgun.ValidateScriptScope()) continue;

      local scope = pgun.GetScriptScope();
      if ("ppmod_onportal_attack_time" in scope) continue;

      scope.ppmod_onportal_attack_time <- 0.0;
      scope.ppmod_onportal_attack2_time <- 0.0;

      pgun.__KeyValueFromString("OnFiredPortal1", "!self\x001BRunScriptCode\x001Bself.GetScriptScope().ppmod_onportal_attack_time<-Time()\x001B0\x001B-1");
      pgun.__KeyValueFromString("OnFiredPortal2", "!self\x001BRunScriptCode\x001Bself.GetScriptScope().ppmod_onportal_attack2_time<-Time()\x001B0\x001B-1");

    }

    local portal = null;
    while (portal = Entities.FindByClassname(portal, "prop_portal")) {

      if (!portal.IsValid()) continue;
      if (!portal.ValidateScriptScope()) continue;

      local scope = portal.GetScriptScope();
      if ("ppmod_onportal_flag" in scope) continue;

      scope["ppmod_onportal_flag"] <- true;

      portal.__KeyValueFromString("OnPlacedSuccessfully", "!self\x001BRunScriptCode\x001Bppmod.onportal_check(self,false)\x001B0\x001B-1");
      ppmod.onportal_check(portal, true);

    }

  });

}

/*******************/
// World interface //
/*******************/

::ppmod.create <- function (cmd, key = null) {

  // The key is the string used to look for the entity after spawning
  // If no key is provided, we guess it from the input command
  if (!key) {

    switch (cmd.slice(0, min(cmd.len(), 17))) {

      case "ent_create_portal": key = "cube"; break;
      case "ent_create_paint_": key = "prop_paint_bomb"; break;

      default:
        if (cmd.find(" ")) { // In most cases, a space means the command argument is a valid key
          key = cmd.slice(cmd.find(" ") + 1);
          if (key.slice(-4) == ".mdl") key = "models/" + key;
        } else if (cmd.slice(-4) == ".mdl") { // If provided only a model, assume we're using 'prop_dynamic_create'
          key = "models/" + cmd;
          cmd = "prop_dynamic_create " + cmd;
        } else { // If all else fails, assume we're given an entity classname, therefore use 'ent_create'
          key = cmd;
          cmd = "ent_create " + cmd;
        }

    }
  }

  SendToConsole(cmd);

  return ppromise(function (resolve, reject):(cmd, key) {

    // Find the entity by passing key to ppmod.prev
    // We send this as a console command to take advantage of how console commands are executed synchronously
    // This lets us make sure that the entity has spawned and that we're looking for it right away
    SendToConsole("script ppmod.scrq_get(" + ppmod.scrq_add(resolve, 1) + ")(ppmod.prev(\"" + key + "\"))");

  });

}

::ppmod.give <- function (classname, amount = 1) {

  local player = Entities.FindByClassname(null, "player");
  local equip = Entities.CreateByClassname("game_player_equip");

  // The keyvalue sets the entity classname and amount
  equip.__KeyValueFromInt(classname, amount);
  EntFireByHandle(equip, "Use", "", 0.0, player, null);

  return ppromise(function (resolve, reject):(classname, amount, equip) {

    local scrq_id = ppmod.scrq_add(function ():(resolve, classname, amount) {

      local arr = array(amount);
      local curridx = 0;

      local ent = null;
      while (ent = Entities.FindByClassname(ent, classname)) {
        arr[curridx] = ent;
        if (++curridx == amount) curridx = 0;
      }

      resolve(arr);

    }, 1);

    EntFireByHandle(equip, "RunScriptCode", "ppmod.scrq_get(" + scrq_id + ")()", 0.0, null, null);
    EntFireByHandle(equip, "Kill", "", 0.0, null, null);

  });

}

::ppmod.brush <- function (pos, size, type = "func_brush", ang = Vector(), create = false) {

  if (!create) {

    local brush = type;
    if (typeof type == "string") brush = Entities.CreateByClassname(type);

    brush.__KeyValueFromInt("Solid", 3);
    brush.SetAbsOrigin(pos);
    brush.SetAngles(ang.x, ang.y, ang.z);
    brush.SetSize(Vector() - size, size);

    return brush;

  }

  return ppromise(function (resolve, reject):(type, pos, size, ang) {

    ppmod.create(type).then(function (ent):(pos, size, ang, resolve) {

      resolve(ppmod.brush(pos, size, ent, ang));

    });

  });

}

::ppmod.trigger <- function (pos, size, type = "trigger_once", ang = Vector(), create = false) {

  if (!create) {

    local trigger = type;
    if (typeof type == "string") trigger = ppmod.brush(pos, size, type, ang);

    trigger.__KeyValueFromInt("CollisionGroup", 10);
    trigger.__KeyValueFromInt("SpawnFlags", 1);
    EntFireByHandle(trigger, "Enable", "", 0.0, null, null);

    if (type == "trigger_once") {
      trigger.__KeyValueFromString("OnStartTouch", "!self\x001BKill\x1B\x001B0\x001B1");
    }

    return trigger;

  }

  return ppromise(function (resolve, reject):(pos, size, type, ang) {

    ppmod.brush(pos, size, type, ang, true).then(function (ent):(pos, size, ang, resolve) {

      resolve(ppmod.trigger(pos, size, ent, ang));

    });

  });

}

::ppmod.project <- function (material, pos, ang = Vector(90, 0, 0), simple = 0, far = 128) {

  local texture = Entities.CreateByClassname("env_projectedtexture");

  texture.SetAbsOrigin(pos);
  texture.SetAngles(ang.x, ang.y, ang.z);
  texture.__KeyValueFromInt("FarZ", far);
  texture.__KeyValueFromInt("SimpleProjection", simple.tointeger());
  texture.__KeyValueFromString("TextureName", material);

  return texture;

}

::ppmod.decal <- function (material, pos, ang = Vector(90, 0, 0)) {

  local decal = Entities.CreateByClassname("infodecal");

  decal.SetAbsOrigin(pos);
  decal.SetAngles(ang.x, ang.y, ang.z);
  decal.__KeyValueFromString("Texture", material);
  EntFireByHandle(decal, "Activate", "", 0.0, null, null);

  return decal;

}

// Set up some dummy entites for simplifying ray-through-portal calculations
local p_anchor = Entities.CreateByClassname("info_target");
local r_anchor = Entities.CreateByClassname("info_target");

p_anchor.__KeyValueFromString("Targetname", "ppmod_portals_p_anchor");
r_anchor.__KeyValueFromString("Targetname", "ppmod_portals_r_anchor");

EntFireByHandle(r_anchor, "SetParent", "ppmod_portals_p_anchor", 0.0, null, null);

::ppmod.ray <- function (start, end, ent = null, world = true, portals = null, ray = null) {

  local formatreturn = function (fraction, ray, hitent = null):(start, end, ent, world, portals) {

    if (world) fraction = min(fraction, TraceLine(start, end, null));
    local dirvec = end - start;

    local output = {
      fraction = fraction,
      point = start + dirvec * fraction,
      entity = hitent
    };

    if (!portals) return output;
    if (typeof portals != "array") return output;
    if (portals.len() < 2) return output;

    // Check if we're intersecting the bounding box of one of the provided portals
    local portal = Entities.FindByClassnameWithin(null, "prop_portal", output.point, 1.0);
    if (portal != portals[0] && portal != portals[1]) return output;

    // Determine which portal is the other portal
    local other = (portal == portals[0]) ? portals[1] : portals[0];

    local p_anchor = Entities.FindByName(null, "ppmod_portals_p_anchor");
    local r_anchor = Entities.FindByName(null, "ppmod_portals_r_anchor");

    // Set portal anchor facing the entry portal
    p_anchor.SetForwardVector(Vector() - portal.GetForwardVector());

    // Set positions of anchors to entry portal origin and ray endpoint, respectively
    p_anchor.SetAbsOrigin(portal.GetOrigin());
    r_anchor.SetAbsOrigin(output.point);

    // Translate both anchor points to exit portal (r_anchor is parented to p_anchor)
    p_anchor.SetAbsOrigin(other.GetOrigin());

    // Calculate angles from vector of ray direction
    // First, normalize the vector to get a unit vector
    local len = dirvec.Norm();

    // Then, calculate yaw, pitch and roll in degrees
    local yaw = atan2(dirvec.y, dirvec.x) / PI * 180;
    local pitch = asin(-dirvec.z) / PI * 180;
    local roll = atan2(dirvec.z, sqrt(dirvec.x * dirvec.x + dirvec.y * dirvec.y)) / PI * 180;

    // Due to being parented, r_anchor's angles are usually relative to p_anchor
    // The "angles" keyvalue, however, is absolute
    r_anchor.__KeyValueFromString("angles", pitch + " " + yaw + " " + roll);
    // Finally, rotate the portal anchor to get ray starting position and direction
    p_anchor.SetForwardVector(other.GetForwardVector());

    local newstart = r_anchor.GetOrigin();

    // Check if the new starting point is behind the exit portal
    local offset = newstart - other.GetOrigin();
    local epsilon = 0.000001; // Some flat walls are not flat...

    if (other.GetForwardVector().x > epsilon && offset.x < -epsilon) return output;
    if (other.GetForwardVector().x < -epsilon && offset.x > epsilon) return output;

    if (other.GetForwardVector().y > epsilon && offset.y < -epsilon) return output;
    if (other.GetForwardVector().y < -epsilon && offset.y > epsilon) return output;

    if (other.GetForwardVector().z > epsilon && offset.z < -epsilon) return output;
    if (other.GetForwardVector().z < -epsilon && offset.z > epsilon) return output;

    local newend = r_anchor.GetOrigin() + r_anchor.GetForwardVector() * (len * (1.0 - fraction));

    return ppmod.ray(newstart, newend, ent, world, portals);

  };

  if (!ent) return formatreturn( 1.0, ray );

  local len, div;
  if (!ray) {
    local dir = end - start;
    len = dir.Norm();
    div = [1.0 / dir.x, 1.0 / dir.y, 1.0 / dir.z];
  } else {
    len = ray[0];
    div = ray[1];
  }

  // Defines behavior when multiple valid entries are provided - returns the lowest fraction among them
  if (typeof ent == "array") {

    // If an array contains only two Vectors, treat those instead as the origin point and half-widths of an entity, respectively
    local isbbox = false;
    if (ent.len() == 2) if (typeof ent[0] == "Vector" && typeof ent[1] == "Vector") {

      local pos = ent[0], size = ent[1];

      ent = {
        GetOrigin = function ():(pos) { return pos },
        GetAngles = function () { return Vector() },
        GetBoundingMaxs = function ():(size) { return size },
        GetBoundingMins = function ():(size) { return Vector() - size },
      };

      isbbox = true;

    }

    // Squirrel sucks, we can't just have an 'else' here
    if (!isbbox) {

      local closest = ppmod.ray(start, end, ent[0], false, portals, [len, div]);
      for (local i = 1; i < ent.len(); i ++) {
        local curr = ppmod.ray(start, end, ent[i], false, portals, [len, div]);
        if (curr.fraction < closest.fraction) closest = curr;
      }
      return formatreturn( closest.fraction, [len, div], closest.entity );

    }

  } else if (typeof ent == "string") {

    local next = ppmod.get(ent);
    local closest = ppmod.ray(start, end, next, false, portals, [len, div]);
    while (next = ppmod.get(ent, next)) {
      local curr = ppmod.ray(start, end, next, false, portals, [len, div]);
      if (curr.fraction < closest.fraction) closest = curr;
    }
    return formatreturn( closest.fraction, [len, div], closest.entity );

  }

  local pos = ent.GetOrigin();

  local mins = ent.GetBoundingMins();
  local maxs = ent.GetBoundingMaxs();

  local minmin = min(mins.x, min(mins.y, mins.z));
  local maxmax = max(maxs.x, max(maxs.y, maxs.z));

  if (pos.x + minmin > max(start.x, end.x)) return formatreturn( 1.0, [len, div] );
  if (pos.x + maxmax < min(start.x, end.x)) return formatreturn( 1.0, [len, div] );

  if (pos.y + minmin > max(start.y, end.y)) return formatreturn( 1.0, [len, div] );
  if (pos.y + maxmax < min(start.y, end.y)) return formatreturn( 1.0, [len, div] );

  if (pos.z + minmin > max(start.z, end.z)) return formatreturn( 1.0, [len, div] );
  if (pos.z + maxmax < min(start.z, end.z)) return formatreturn( 1.0, [len, div] );

  local ang = ent.GetAngles() * (PI / 180.0);
  local c1 = cos(ang.z);
  local s1 = sin(ang.z);
  local c2 = cos(ang.x);
  local s2 = sin(ang.x);
  local c3 = cos(ang.y);
  local s3 = sin(ang.y);

  local matrix = [
    [c2 * c3, c3 * s1 * s2 - c1 * s3, s1 * s3 + c1 * c3 * s2],
    [c2 * s3, c1 * c3 + s1 * s2 * s3, c1 * s2 * s3 - c3 * s1],
    [-s2, c2 * s1, c1 * c2]
  ];

  mins = [mins.x, mins.y, mins.z];
  maxs = [maxs.x, maxs.y, maxs.z];

  local bmin = [pos.x, pos.y, pos.z];
  local bmax = [pos.x, pos.y, pos.z];
  local a, b;

  for (local i = 0; i < 3; i ++) {
    for (local j = 0; j < 3; j ++) {
      a = (matrix[i][j] * mins[j]);
      b = (matrix[i][j] * maxs[j]);
      if(a < b) {
        bmin[i] += a;
        bmax[i] += b;
      } else {
        bmin[i] += b;
        bmax[i] += a;
      }
    }
  }

  if (
    start.x > bmin[0] && start.x < bmax[0] &&
    start.y > bmin[1] && start.y < bmax[1] &&
    start.z > bmin[2] && start.z < bmax[2]
  ) return formatreturn( 0.0, [len, div], ent );

  start = [start.x, start.y, start.z];

  local tmin = [0.0, 0.0, 0.0];
  local tmax = [0.0, 0.0, 0.0];

  for (local i = 0; i < 3; i ++) {
    if (div[i] >= 0) {
      tmin[i] = (bmin[i] - start[i]) * div[i];
      tmax[i] = (bmax[i] - start[i]) * div[i];
    } else {
      tmin[i] = (bmax[i] - start[i]) * div[i];
      tmax[i] = (bmin[i] - start[i]) * div[i];
    }
    if (tmin[0] > tmax[i] || tmin[i] > tmax[0]) return formatreturn( 1.0, [len, div] );
    if (tmin[i] > tmin[0]) tmin[0] = tmin[i];
    if (tmax[i] < tmax[0]) tmax[0] = tmax[i];
  }

  if (tmin[0] < 0) tmin[0] = 1.0;
  else tmin[0] /= len;

  return formatreturn( tmin[0], [len, div], ent );

}

::ppmod.inbounds <- function (point) {

  if (TraceLine(point, point + Vector(65536, 0, 0), null) == 1.0) return false;
  if (TraceLine(point, point - Vector(65536, 0, 0), null) == 1.0) return false;
  if (TraceLine(point, point + Vector(0, 65536, 0), null) == 1.0) return false;
  if (TraceLine(point, point - Vector(0, 65536, 0), null) == 1.0) return false;
  if (TraceLine(point, point + Vector(0, 0, 65536), null) == 1.0) return false;
  if (TraceLine(point, point - Vector(0, 0, 65536), null) == 1.0) return false;

  return true;

}

::ppmod.button <- function (type, pos, ang = Vector()) {

  // Ensure that sounds are precached by creating a dummy entity
  ppmod.create(type).then(function (dummy) {
    dummy.Destroy();
  });

  local model;

  if (type == "prop_button") model = "props/switch001.mdl";
  if (type == "prop_under_button") model = "props_underground/underground_testchamber_button.mdl";
  if (type == "prop_floor_button") model = "props/portal_button.mdl";
  if (type == "prop_floor_cube_button") model = "props/box_socket.mdl";
  if (type == "prop_floor_ball_button") model = "props/ball_button.mdl";
  if (type == "prop_under_floor_button") model = "props_underground/underground_floor_button.mdl";

  return ppromise(function (resolve, reject):(type, pos, ang, model) {

    // First, create a prop_dynamic with the appropriate model
    ppmod.create(model).then(function (ent):(type, pos, ang, resolve) {

      ent.SetAbsOrigin(pos);
      ent.SetAngles(ang.x, ang.y, ang.z);

      // The floor buttons often come with additional phys_bone_followers
      while (ent.GetClassname() == "phys_bone_follower") {
        ent = ppmod.prev(ent.GetModelName(), ent);
        ent.SetAbsOrigin(pos);
        ent.SetAngles(ang.x, ang.y, ang.z);
      }

      if (type == "prop_button" || type == "prop_under_button") { // Handle pedestal buttons

        // func_button seems to be broken when spawned during runtime, hence the use of func_rot_button
        ppmod.brush(pos + (ent.GetUpVector() * 40), Vector(8, 8, 8), "func_rot_button", ang, true).then(function (button):(type, ent, resolve) {

          // Make the button box non-solid and activated with +use
          button.__KeyValueFromInt("CollisionGroup", 2);
          button.__KeyValueFromInt("SpawnFlags", 1024);
          ppmod.setparent(button, ent);

          // Properties are stored in the func_rot_button's script scope
          button.ValidateScriptScope();
          button.GetScriptScope()["button_delay"] <- 1.0;
          button.GetScriptScope()["button_timer"] <- false;
          button.GetScriptScope()["button_permanent"] <- false;

          ppmod.addscript(button, "OnPressed", function ():(type, ent, button) {

            // Underground buttons have different animation names
            // The additional sound effects for those are baked into the animation
            if (type == "prop_button") EntFireByHandle(ent, "SetAnimation", "down", 0.0, null, null);
            else EntFireByHandle(ent, "SetAnimation", "press", 0.0, null, null);
            button.EmitSound("Portal.button_down");

            // To disable the button while it's down, we clear its "+use activates" flag
            button.__KeyValueFromInt("SpawnFlags", 0);

            local timer = null; // Simulate the timer ticks
            if (button.GetScriptScope()["button_timer"]) {

              timer = Entities.CreateByClassname("logic_timer");

              ppmod.addscript(timer, "OnTimer", function ():(button) {
                button.EmitSound("Portal.room1_TickTock");
              });

              // Offset activation by one tick to prevent an extra tick upon release
              EntFireByHandle(timer, "RefireTime", "1", 0.0, null, null);
              EntFireByHandle(timer, "Enable", "", FrameTime(), null, null);

            }

            // If "permanent", skip the release code
            if (button.GetScriptScope()["button_permanent"]) return;

            ppmod.wait(function ():(ent, button, type, timer) {

              if (type == "prop_button") EntFireByHandle(ent, "SetAnimation", "up", 0.0, null, null);
              else EntFireByHandle(ent, "SetAnimation", "release", 0.0, null, null);
              button.EmitSound("Portal.button_up");

              button.__KeyValueFromInt("SpawnFlags", 1024);
              if (timer) timer.Destroy();

            }, button.GetScriptScope()["button_delay"]);

          });

          resolve({

            GetButton = function ():(button) { return button },
            GetProp = function ():(ent) { return ent },
            SetDelay = function (delay):(button) { button.GetScriptScope()["button_delay"] <- delay },
            SetTimer = function (enabled):(button) { button.GetScriptScope()["button_timer"] <- enabled },
            SetPermanent = function (enabled):(button) { button.GetScriptScope()["button_permanent"] <- enabled },
            OnPressed = function (scr):(button) { ppmod.addscript(button, "OnPressed", scr) },

          });

        });

      } else { // Handle floor buttons

        // This moves the phys_bone_followers into place
        EntFireByHandle(ent, "SetAnimation", "BindPose", 0.0, null, null);

        local trigger;
        if (type == "prop_under_floor_button") {
          trigger = ppmod.trigger(pos + Vector(0, 0, 8.5), Vector(30, 30, 8.5), "trigger_multiple", ang);
        } else {
          trigger = ppmod.trigger(pos + Vector(0, 0, 7), Vector(20, 20, 7), "trigger_multiple", ang);
        }

        // Activated by players and physics props
        trigger.__KeyValueFromInt("SpawnFlags", 9);

        trigger.ValidateScriptScope();
        trigger.GetScriptScope()["count"] <- 0;

        // Used for attaching output scripts to press and unpress events
        local pressrl = Entities.CreateByClassname("logic_relay");
        pressrl.__KeyValueFromInt("SpawnFlags", 2);
        local unpressrl = Entities.CreateByClassname("logic_relay");
        unpressrl.__KeyValueFromInt("SpawnFlags", 2);

        local press = function ():(type, trigger, ent, pressrl, unpressrl) {
          if (++trigger.GetScriptScope()["count"] == 1) {

            EntFireByHandle(pressrl, "Trigger", "", 0.0, null, null);

            if (type == "prop_under_floor_button") {
              EntFireByHandle(ent, "SetAnimation", "press", 0.0, null, null);
              ent.EmitSound("Portal.OGButtonDepress");
            } else {
              EntFireByHandle(ent, "SetAnimation", "down", 0.0, null, null);
              ent.EmitSound("Portal.ButtonDepress");
            }

          }
        };

        local unpress = function ():(type, trigger, ent) {
          if (--trigger.GetScriptScope()["count"] == 0) {

            EntFireByHandle(unpressrl, "Trigger", "", 0.0, null, null);

            if (type == "prop_under_floor_button") {
              EntFireByHandle(ent, "SetAnimation", "release", 0.0, null, null);
              ent.EmitSound("Portal.OGButtonRelease");
            } else {
              EntFireByHandle(ent, "SetAnimation", "up", 0.0, null, null);
              ent.EmitSound("Portal.ButtonRelease");
            }

          }
        };

        // Checks classnames and model names to filter the entities activating the button
        local strpress, strunpress;
        if (type == "prop_floor_button" || type == "prop_under_floor_button") {
          strpress = "if (self.GetClassname() == \"prop_weighted_cube\" || self.GetClassname() == \"player\") ppmod.scrq_get(" + ppmod.scrq_add(press) + ")()";
          strunpress = "if (self.GetClassname() == \"prop_weighted_cube\" || self.GetClassname() == \"player\") ppmod.scrq_get(" + ppmod.scrq_add(unpress) + ")()";
        } else if (type == "prop_floor_ball_button") {
          strpress = "if (self.GetClassname() == \"prop_weighted_cube\" && self.GetModelName() == \"models/props_gameplay/mp_ball.mdl\") ppmod.scrq_get(" + ppmod.scrq_add(press) + ")()";
          strunpress = "if (self.GetClassname() == \"prop_weighted_cube\" && self.GetModelName() == \"models/props_gameplay/mp_ball.mdl\") ppmod.scrq_get(" + ppmod.scrq_add(unpress) + ")()";
        } else {
          strpress = "if (self.GetClassname() == \"prop_weighted_cube\" && self.GetModelName() != \"models/props_gameplay/mp_ball.mdl\") ppmod.scrq_get(" + ppmod.scrq_add(press) + ")()";
          strunpress = "if (self.GetClassname() == \"prop_weighted_cube\" && self.GetModelName() != \"models/props_gameplay/mp_ball.mdl\") ppmod.scrq_get(" + ppmod.scrq_add(unpress) + ")()";
        }

        ppmod.addoutput(trigger, "OnStartTouch", "!activator", "RunScriptCode", strpress);
        ppmod.addoutput(trigger, "OnEndTouch", "!activator", "RunScriptCode", strunpress);

        resolve({

          GetTrigger = function ():(trigger) { return trigger },
          GetProp = function ():(ent) { return ent },
          GetCount = function ():(trigger) { return trigger.GetScriptScope()["count"] },
          OnPressed = function (scr):(pressrl) { ppmod.addscript(pressrl, "OnTrigger", scr) },
          OnUnpressed = function (scr):(unpressrl) { ppmod.addscript(unpressrl, "OnTrigger", scr) },

        });

      }

    });

  });

}

::ppmod.catapult <- function (ent, vec) {

  if (!(typeof ent == "instance" && ent instanceof CBaseEntity)) {
    ppmod.getall(ent, function (curr):(vec) {
      ppmod.catapult(curr, vec);
    });
    return;
  }

  local speed = vec.Norm();

  local trigger = Entities.CreateByClassname("trigger_catapult");
  trigger.__KeyValueFromInt("Solid", 3);
  trigger.SetAbsOrigin(ent.GetOrigin());
  trigger.SetForwardVector(vec);
  trigger.SetSize(Vector(-0.2, -0.2, -0.2), Vector(0.2, 0.2, 0.2));
  trigger.__KeyValueFromInt("CollisionGroup", 1);

  local ang = trigger.GetAngles();
  trigger.__KeyValueFromInt("SpawnFlags", 8);
  trigger.__KeyValueFromFloat("PhysicsSpeed", speed);
  trigger.__KeyValueFromString("LaunchDirection", ang.x+" "+ang.y+" "+ang.z);

  EntFireByHandle(trigger, "Enable", "", 0.0, null, null);
  EntFireByHandle(trigger, "Kill", "", 0.0, null, null);

}

/******************/
// Game interface //
/******************/

::ppmod.text <- class {

  ent = null;

  constructor (text = "", x = -1.0, y = -1.0) {
    this.ent = Entities.CreateByClassname("game_text");
    this.ent.__KeyValueFromString("Message", text);
    this.ent.__KeyValueFromString("Color", "255 255 255");
    this.ent.__KeyValueFromFloat("X", x);
    this.ent.__KeyValueFromFloat("Y", y);
  }

  function GetEntity () {
    return this.ent;
  }
  function SetPosition (x, y) {
    this.ent.__KeyValueFromFloat("X", x);
    this.ent.__KeyValueFromFloat("Y", y);
  }
  function SetText (text) {
    this.ent.__KeyValueFromString("Message", text);
  }
  function SetSize (size) {
    // Channels sorted from smallest to biggest font size
    this.ent.__KeyValueFromInt("Channel", [2, 1, 4, 0, 5, 3][size]);
  }
  function SetColor (c1, c2 = null) {
    this.ent.__KeyValueFromString("Color", c1);
    if (c2) this.ent.__KeyValueFromString("Color2", c2);
  }
  function SetFade (fin, fout, fx = false) {
    this.ent.__KeyValueFromFloat("FadeIn", fin);
    this.ent.__KeyValueFromFloat("FXTime", fin);
    this.ent.__KeyValueFromFloat("FadeOut", fout);
    if (fx) this.ent.__KeyValueFromInt("Effect", 2);
    else this.ent.__KeyValueFromInt("Effect", 0);
  }
  function Display (hold = null, player = null) {
    if (hold == null) hold = FrameTime();
    this.ent.__KeyValueFromFloat("HoldTime", hold);
    if (player) this.ent.__KeyValueFromInt("SpawnFlags", 0);
    else this.ent.__KeyValueFromInt("SpawnFlags", 1);
    EntFireByHandle(ent, "Display", "", 0.0, player, null);
  }

}

::ppmod.fwrite <- function (path, str) { // EXPERIMENTAL

  printl("[ppmod] Warning: fwrite is an experimental feature!");

  local stall = "";
  for (local i = 195; i > path.len(); i --) stall += "/";

  if (path[0] == "/") path = stall + path;
  else path = "." + stall.slice(1) + path;

  for (local i = 0; i < str.len(); i ++) {
    if (str[i] == '\\') str = str.slice(0, i) + "\\\\" + str.slice(++i);
    if (str[i] == '"') str = str.slice(0, i) + "\\\x22" + str.slice(++i);
  }

  SendToConsole("con_logfile \"" + path + ".log\"");
  SendToConsole("script print(\"" + str + "\")");
  SendToConsole("con_logfile \"\"");

}

// ::ppmod.conread <- function (command, callback) { // EXPERIMENTAL

//   printl("[ppmod] Warning: conread is an experimental feature!");

//   local path = ".//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////scripts/vscripts/conread.nut";

//   local scr = "ppmod.scrq_get(" + ppmod.scrq_add(callback, 1) + ")";

//   // SendToConsole("writeip");
//   SendToConsole("con_logfile \"" + path + ".log\"");
//   SendToConsole("script print(\"local output = @\\\"\")");
//   SendToConsole(command);
//   SendToConsole("script printl(\"\\\"\")");
//   SendToConsole("script printl(\""+scr+"(output)\")");
//   SendToConsole("con_logfile \"\"");

//   SendToConsole("script_execute conread");

// }
