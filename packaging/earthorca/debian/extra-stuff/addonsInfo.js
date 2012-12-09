const Cc = Components.classes;
const Ci = Components.interfaces;
Components.utils.import("resource://gre/modules/Services.jsm");
Components.utils.import("resource://gre/modules/XPCOMUtils.jsm");
Components.utils.import("resource://gre/modules/AddonManager.jsm");

function compare(a, b) {
  return String.localeCompare(a.name, b.name);
}

function dumper() {}
dumper.prototype = {
  close: function() { },
  writeString: function(str) {
    dump(str);
  }
};

function dump_addons(out) {
  var addons = false;
  AddonManager.getAllAddons(function(aAddons) {
    addons = aAddons;
  });

  var thread = Services.tm.currentThread;
  while (addons == false) {
    thread.processNextEvent(true);
  }

  addons.sort(compare);
  out.writeString("-- Extensions information\n");
  addons.forEach(function(extension) {
    if (extension.type == "plugin")
      return;
    out.writeString("Name: " + extension.name);
    if (extension.type != "extension")
      out.writeString(" " + extension.type);
    out.writeString("\n");
    if (extension.getResourceURI) {
      var location = extension.getResourceURI("").QueryInterface(Ci.nsIFileURL).file;
      if (extension.scope == AddonManager.SCOPE_PROFILE)
        out.writeString("Location: ${PROFILE_EXTENSIONS}/" +
                        location.leafName + "\n");
      else
        out.writeString("Location: " + location.path + "\n");
    }
    out.writeString("Status: " + (extension.appDisabled ? "app-disabled" :
                                 (extension.userDisabled ? "user-disabled" :
                                 "enabled")) + "\n");
    out.writeString("\n");
  });

  var phs = Cc["@mozilla.org/plugin/host;1"]
            .getService(Ci.nsIPluginHost);
  var plugins = phs.getPluginTags({ });
  plugins.sort(compare);
  out.writeString("-- Plugins information\n");
  plugins.forEach(function(plugin) {
    out.writeString("Name: " + plugin.name +
           (plugin.version ? " (" + plugin.version + ")" : "") + "\n");
    out.writeString("Location: " +
           (plugin.fullpath ? plugin.fullpath : plugin.filename) + "\n");
    out.writeString("Status: " + (plugin.disabled ? "disabled" : "enabled") +
                    (plugin.blocklisted ? " blocklisted" : "") + "\n");
    out.writeString("\n");
  });
}

function addonsInfoHandler() {}
addonsInfoHandler.prototype = {
  handle: function clh_handle(cmdLine) {
    var path;
    var out;
    try {
      path = cmdLine.handleFlagWithParam("dump-addons-info", false);
      if (!path)
        return;
    } catch(e) {
      if (!cmdLine.handleFlag("dump-addons-info", false))
        return;
    }

    cmdLine.preventDefault = true;

    if (path) {
      var file = Cc["@mozilla.org/file/local;1"]
                 .createInstance(Ci.nsILocalFile);
      file.initWithPath(path);
      var outstream = Cc["@mozilla.org/network/file-output-stream;1"]
                      .createInstance(Ci.nsIFileOutputStream);
      outstream.init(file, 0x2A /* TRUNCATE | WRONLY | CREATE */, 0666, 0);
      out = Cc["@mozilla.org/intl/converter-output-stream;1"]
            .createInstance(Ci.nsIConverterOutputStream);
      out.init(outstream, "UTF-8", 0, 0);
    } else
      out = new dumper();

    dump_addons(out);
    out.close();
  },

  classDescription: "addonsInfoHandler",
  classID: Components.ID("{17a1f091-70f7-411c-a9d7-191689552d01}"),
  contractID: "@mozilla.org/toolkit/addonsInfo-clh;1",
  QueryInterface: XPCOMUtils.generateQI([Ci.nsICommandLineHandler]),
};

const NSGetFactory = XPCOMUtils.generateNSGetFactory([addonsInfoHandler]);
