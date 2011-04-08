{application,
 sleeping_barber,
 [
  {description,"A sleeping barber simulation"},
  {vsn, "1.0"},
  {modules, [sleeping_barber, sleeping_barber_sup, room, barber]},
  {registered, [room, barber]},
  {applications, [kernel, stdlib]},
  {mod, {sleeping_barber,[]}},
  {start_phases,[]}
  ]}.
