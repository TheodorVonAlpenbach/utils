signal time=1782309915.231050 sender=org.freedesktop.DBus -> destination=:1.317 serial=2 path=/org/freedesktop/DBus; interface=org.freedesktop.DBus; member=NameAcquired
   string ":1.317"
signal time=1782309915.231131 sender=org.freedesktop.DBus -> destination=:1.317 serial=4 path=/org/freedesktop/DBus; interface=org.freedesktop.DBus; member=NameLost
   string ":1.317"
method call time=1782309917.267562 sender=:1.318 -> destination=:1.46 serial=7 path=/org/freedesktop/Notifications; interface=org.freedesktop.Notifications; member=GetServerInformation
method call time=1782309917.269621 sender=:1.46 -> destination=:1.40 serial=627 path=/org/freedesktop/Notifications; interface=org.freedesktop.Notifications; member=GetServerInformation
method call time=1782309917.275239 sender=:1.318 -> destination=:1.46 serial=8 path=/org/freedesktop/Notifications; interface=org.freedesktop.Notifications; member=GetServerInformation
method call time=1782309917.276051 sender=:1.46 -> destination=:1.40 serial=631 path=/org/freedesktop/Notifications; interface=org.freedesktop.Notifications; member=GetServerInformation
method call time=1782309917.278466 sender=:1.318 -> destination=:1.46 serial=9 path=/org/freedesktop/Notifications; interface=org.freedesktop.Notifications; member=Notify
   string "notify-send"
   uint32 0
   string ""
   string "Test"
   string "Dette er en test taskId=12345 fra terminal"
   array [
   ]
   array [
      dict entry(
         string "urgency"
         variant             byte 1
      )
      dict entry(
         string "sender-pid"
         variant             int64 65011
      )
   ]
   int32 -1
method call time=1782309917.281539 sender=:1.46 -> destination=:1.40 serial=634 path=/org/freedesktop/Notifications; interface=org.freedesktop.Notifications; member=Notify
   string "notify-send"
   uint32 0
   string ""
   string "Test"
   string "Dette er en test taskId=12345 fra terminal"
   array [
   ]
   array [
      dict entry(
         string "urgency"
         variant             byte 1
      )
      dict entry(
         string "sender-pid"
         variant             int64 65011
      )
      dict entry(
         string "x-shell-sender-pid"
         variant             uint32 65011
      )
      dict entry(
         string "x-shell-sender"
         variant             string ":1.318"
      )
   ]
   int32 -1
