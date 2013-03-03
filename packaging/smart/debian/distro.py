
if not sysconf.getReadOnly():
    if not sysconf.has("sync-apt-sources"):
        sysconf.set("sync-apt-sources", True, weak=True)
    if not sysconf.has("detect-sys-channels"):
        sysconf.set("detect-sys-channels", "deb", weak=True)
    if sysconf.has("channels.rpm-sys"):
        if sysconf.get("channels.rpm-sys.name") == "RPM System":
            # Likely auto-detected in an old installation.  Let's remove it
            # to prevent dependency issues.
            sysconf.remove("channels.rpm-sys")

    # Import proxy information from Landscape if available and not
    # explicitly set in Smart itself.
    sysconf.set("use-landscape-proxies", True, weak=True)
