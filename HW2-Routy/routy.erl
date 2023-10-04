-module(routy).
-compile(export_all).

start(Reg, Name) ->

  register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
  Node ! stop,

  unregister(Node).


init(Name) ->

	Intf = interfaces:new(),

	Map = map:new(),
	

	Table = dijkstra:table(Intf, Map),
	

	Hist = history:new(Name),
	router(Name, 0, Hist, Intf, Table, Map).
% start(Reg, Name) ->
%     register(Reg, spawn(fun() -> init(Name) end)).

% stop(Node) ->
%     Node ! stop,
%     unregister(Node).
% init(Name) ->
%     Intf = Intf:new(),
%     Map = Map:new(),
%     Table = dijkstra:table(Intf, Map),
%     Hist = historyoryoryory:new(Name),
%     router(Name, 0, Msgs, Intf, Table, Map).

% router(Name, N, History, Intf, Table, Map) ->
%     recieve
%         {add, Node, Pid} ->
%             Reference = erlang:monitor(process, Pid),
%             InterfaceOne = interfaces:add(Node, Reference, Pid, Intf),
%             router(Name, N, InterfaceOne, Table, Map);
%         {remove, Node} ->
%             {ok, Reference} = interfaces:ref(Node, Intf),
%             erlang:demonitor(Reference),
%             InterfaceOne = interfaces:remove(Node, Intf),
%             router(Name, N, History, InterfaceOne, Table, Map);
%         {'DOWN', Ref, process, _, _} ->
%             {ok, Down} = interfaces:name(Ref, Intf),
%             io:format("~w: exit recieved from ~w~n", [Name, Down]),
%             InterfaceOne = interfaces:remove(Down, Intf),
%             router(Name, N, History, InterfaceOne, Table, Map);
%         {links, Node, R, Links} ->
%             case historyoryoryory:update(Node, R, History) of
%                 {new, HistoryOne} ->
%                     interfaces:broadcast({links, Node, R, Links}, Intf),
%                     MapOne = map:update(Node, Links, Map),
%                     router(Name, N, HistoryOne, Intf, Table, MapOne);
%                 old ->
%                     router(Name, N, History, Intf, Table, Map)
%                 end;
%         update ->
%             TableOne = dijkstra:table(interfaces:list(Intf), Map),
%             router(Name, N, History, Intf, TableOne, Map);
%         broadcast ->
%             Message = {links, Name, N, interfaces:list(Intf)},
%             interfaces:broadcast(Message, Intf),
%             router(Name, N+1, History, Intf, Table, Map);
%         {route, Name, From, Message} -> 
%             io:format("~w: recieved message ~w~n", [Name, Message]),
%             router(Name, N, History, Intf, Table, Map);
%         {route, To, From, Message} ->
%             io:format("~w: routing message (~w)", [Name, Message]),
%             case dijkstra:route(To, Table) of
%                 {ok, Gw} ->
%                     case intf:lookup(Gw, Intf) of
%                         {ok, Pid} ->
%                             Pid ! {route, To, From, Message};
%                         notfound ->
%                             ok
%                     end;
%                 notfound ->
%                     ok
%             end,
%             router(Name, N, Hist, Intf, Table, Map);
%         {send, To, Message} ->
%             self() ! {route, To, Name, Message},
%             router(Name, N, Hist, Intf, Table, Map);
%         {status, From} ->
%             From ! {status, {Name, N, History, Intf, Table, Map}},
%             router(Name, N, History, Intf, Table, Map);
%         stop ->
%             ok
%         end.
router(Name, N, Hist, Intf, Table, Map) ->
  receive

    {add, Node, Pid} ->

        Ref = erlang:monitor(process, Pid),
		

        Intf1 = interfaces:add(Node, Ref, Pid, Intf),
		

        router(Name, N, Hist, Intf1, Table, Map);

    {remove, Node} ->

        {ok, Ref} = interfaces:ref(Node, Intf),
		

        erlang:demonitor(Ref),
		

        Intf1 = interfaces:remove(Node, Intf),
        router(Name, N, Hist, Intf1, Table, Map);
	  

	{'DOWN', Ref, process, _,_}->

		{ok, Down} = interfaces:name(Ref, Intf),
        io:format("~w: exit recived from ~w~n", [Name, Down]),
		

        Intf1 = interfaces:remove(Down, Intf),
        router(Name, N, Hist, Intf1, Table, Map);
	

	{links, Node, R, Links} ->

		case history:update(Node, R, Hist) of

			{new, Hist1} ->

				interfaces:broadcast({links, Node, R, Links}, Intf),
				

				Map1 = map:update(Node, Links, Map),
				router(Name, N, Hist1, Intf, Table, Map1);
			

			old ->
				router(Name, N, Hist, Intf, Table, Map)
		end;
	  

	update ->
		Table1 = dijkstra:table(interfaces:list(Intf), Map),
		router(Name, N, Hist, Intf, Table1, Map);
	

	broadcast ->

		Message = {links, Name, N, interfaces:list(Intf)},
		interfaces:broadcast(Message, Intf),
		router(Name, N+1, Hist, Intf, Table, Map);
	

	{route, Name, From, Message} ->
		io:format("~w: Received message ~w ~n", [Name, Message]),
		router(Name, N, Hist, Intf, Table, Map);

	{route, To, From, Message} ->
		io:format("~w: routing message of (~w)", [Name, Message]),

		case dijkstra:route(To, Table) of
			{ok, Gw} ->

				case interfaces:lookup(Gw, Intf) of
					{ok, Pid} ->

						Pid ! {route, To, From, Message};
					notfound ->
						ok
				end;
			notfound ->
				ok
		end,
		router(Name, N, Hist, Intf, Table, Map);
	

	{send, To, Message} ->
		self() ! {route, To, Name, Message},
		router(Name, N, Hist, Intf, Table, Map);


    {status, From} ->

        From ! {status, {Name, N, Hist, Intf, Table, Map}},
        router(Name, N, Hist, Intf, Table, Map);
    stop ->
        ok
  end.

requestStatus(Pid)->
	Pid ! {status, self()},
	receive 
		{status, Status}->
			io:format("Status: ~w~n", [Status])
	end. 