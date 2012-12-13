Description: Fix memory leaks in act-user.c
 Caller must free all of the values returned by g_variant_iter_next()
Bug: https://bugs.freedesktop.org/show_bug.cgi?id=51039
Origin: upstream, commit:279a6e0dfaffd6d8aa8363
---
 src/libaccountsservice/act-user.c |    4 +++-
 1 file changed, 3 insertions(+), 1 deletion(-)

--- accountsservice.orig/src/libaccountsservice/act-user.c
+++ accountsservice/src/libaccountsservice/act-user.c
@@ -1063,7 +1063,7 @@ on_get_all_finished (GObject        *obj
         GError      *error;
         GVariant    *res;
         GVariantIter *iter;
-        const gchar *key;
+        gchar *key;
         GVariant    *value;
 
         g_assert (G_IS_DBUS_PROXY (user->object_proxy));
@@ -1089,6 +1089,8 @@ on_get_all_finished (GObject        *obj
         g_variant_get (res, "(a{sv})", &iter);
         while (g_variant_iter_next (iter, "{sv}", &key, &value)) {
                 collect_props (key, value, user);
+                g_free (key);
+                g_variant_unref (value);
         }
         g_variant_iter_free (iter);
         g_variant_unref (res);
