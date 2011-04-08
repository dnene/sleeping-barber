{application,
 sleeping_barber,
 [
  {description,"A sleeping barber simulation"},
  {vsn, "1.0"},
  {modules, [sleeping_barber, sleeping_barber_sup, room, barber, scenario]},
  {registered, [room, barber, sleeping_barber_sup]},
  {applications, [kernel, stdlib]},
  {mod, {sleeping_barber,[]}},
  {start_phases,[]}
  ]}.
