#include "common.h"
#include "frontend.h"
#include "plugin.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <dirent.h>
#include <dlfcn.h>

/* Convert hyphens to underscores. Returns allocated string. */
static char *make_symbol_name(const char *name)
{
    char *symbol, *symbolp;

    symbol = strdup(name);
    for (symbolp = symbol; *symbolp; ++symbolp) {
        if (*symbolp == '-')
            *symbolp = '_';
    }
    return symbol;
}

struct plugin *plugin_new(const char *frontend, const char *filename)
{
    struct plugin *plugin = NEW(struct plugin);
    const char *base;
    size_t baselen, symbollen;
    char *typesymbol, *symbol;

    base = strrchr(filename, '/');
    if (base)
        ++base;
    else
        base = filename;

    baselen = strlen(base);
    /* base must be plugin-<type>.so */
    if (baselen < 11)
        return NULL;
    if (strncmp(base, "plugin-", 7) != 0)
        return NULL;
    if (strncmp(base + baselen - 3, ".so", 3) != 0)
        return NULL;

    plugin->name = malloc(baselen - 9);
    strncpy(plugin->name, base + 7, baselen - 10);
    plugin->name[baselen - 10] = '\0';
    typesymbol = make_symbol_name(plugin->name);

    plugin->module = dlopen(filename, RTLD_LAZY);
    if (plugin->module == NULL) {
        INFO(INFO_WARN, "Cannot load plugin module %s: %s",
             filename, dlerror());
        free(plugin->name);
        DELETE(plugin);
        return NULL;
    }

    symbollen = strlen(frontend) + 18 + strlen(plugin->name) + 1;
    symbol = malloc(symbollen);
    snprintf(symbol, symbollen, "cdebconf_%s_handler_%s", frontend,
             typesymbol);
    plugin->handler = dlsym(plugin->module, symbol);
    free(symbol);
    if (plugin->handler == NULL) {
        /* Let's try with the old style symbol name. */
        symbollen = strlen(frontend) + 9 + strlen(plugin->name) + 1;
        symbol = malloc(symbollen);
        snprintf(symbol, symbollen, "%s_handler_%s", frontend, typesymbol);
        plugin->handler = dlsym(plugin->module, symbol);
        free(symbol);
    }
    if (plugin->handler == NULL) {
        INFO(INFO_WARN, "Malformed plugin module %s", filename);
        plugin_delete(plugin);
        return NULL;
    }

    return plugin;
}

void plugin_delete(struct plugin *plugin)
{
    INFO(INFO_VERBOSE, "Unloading plugin module %s", plugin->name);
    dlclose(plugin->module);
    free(plugin->name);
    DELETE(plugin);
}

struct plugin *plugin_find(struct frontend *frontend, const char *name)
{
    char *filename;
    struct plugin *plugin;

    if (asprintf(&filename, "%s/plugin-%s.so",
                 frontend->plugin_path, name) == -1)
        DIE("Out of memory");
    INFO(INFO_VERBOSE, "Trying to load plugin from %s", filename);
    plugin = plugin_new(frontend->name, filename);
    free(filename);

    return plugin;
}

struct plugin *plugin_iterate(struct frontend *frontend, void **state)
{
    DIR *plugin_dir = *state;
    struct dirent *plugin_dirent;

    if (!plugin_dir) {
        *state = plugin_dir = opendir(frontend->plugin_path);
        if (!plugin_dir) {
            if (errno != ENOENT)
                INFO(INFO_WARN, "Cannot open plugin directory %s: %s",
                     frontend->plugin_path, strerror(errno));
            return NULL;
        }
    }

    while ((plugin_dirent = readdir(plugin_dir)) != NULL) {
        char *filename;
        struct plugin *plugin;

        if (asprintf(&filename, "%s/%s",
                     frontend->plugin_path, plugin_dirent->d_name) == -1)
            DIE("Out of memory");
        plugin = plugin_new(frontend->name, filename);
        free(filename);
        if (plugin)
            return plugin;
    }

    closedir(plugin_dir);
    return NULL;
}
