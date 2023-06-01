-- Program to create a basic control table from track layout information input
-- as CSV tables. This is likely to be subject to scope creep but initially
-- in-route signal to signal and signal to points locking is targeted as being
-- Most critical to safety and least variable in terms of signalling rules.

-- Author    : David Haley
-- Created   : 24/03/2023
-- Last Edit : 30/04/2023
-- 20230430 : Find conflicting roures over Diamonds. Extend locking if the exit
-- end is not clearance.
-- 20230427 : Remove conflicting routes that are locked out by points lie.
-- 20230426 : List opposing routes;
-- 20230423 : Signal Numbers made a string to allow for a prefix nmenonic.
-- Track_Stores and Sub_Route_Lists changed from vector to doubly linked list.
-- Points route holding tracks extended to include next inroute track if
-- Straight or Divergent ends are not clear, similarly for Switch_Diamond.
-- 20230412 : Points Route Holding added to listing.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with CT_Types; use CT_Types;
with Get_Data; use Get_Data;
with Build_Structures; use Build_Structures;
with DJH.Date_and_Time_Strings; use DJH.Date_and_Time_Strings;
use CT_Types.Route_Maps;
use CT_Types.Sub_Route_Lists;

procedure Basic_Ct is

   Solid_Line : constant String := 80 * '_';
   Dotted_Line : constant String := 80 * '.';

   use Route_Stores;

   function Output_Path return String is

      -- Returns the to the path where the output files are to be sent. If the
      -- path does not exist it will attempt to creat it, thie could fail,
      -- causing an exception to be raised.

   begin -- Output_Path
      if Argument_Count = 1 then
         declare -- Path declaration block
            Path : String := Argument (1);
         begin
            if Exists (Path) then
               if Kind (Path) = Directory then
                  return Path;
               else
                  raise Ada.Text_IO.Use_Error with "Path """ & Argument (1) &
                    """ is not a directory";
               end if; -- Kind (Path) = Directory
            else
               Create_Directory (Path);
               return Path;
            end if; -- Exists (Path)
         end; -- Path declaration block
      else
         return "";
      end if; -- Argument_Count = 1
   end Output_Path;

   procedure Header (Output_File : in out File_Type;
                     Route : in Route_Names;
                     Route_Store : in Route_Stores.Map) is

   begin -- Header
      Put_Line ("Writing CT for " & Route);
      Put_Line (Output_File, "Date: " & Date_String & "  Time: " & Time_String);
      Put_Line (Output_File, "Route: " & Route & " from " &
                  Route_Store (Route).Entrance_Signal & " to " &
                  Route_Store (Route).Exit_Signal & " Route class " &
                  Route_Store (Route).Route_Class'Img);
      Put_Line (Output_File, Solid_Line);
   end Header;

   procedure Tracks_Clear (Output_File : in out File_Type;
                           Route : in Route_Names;
                           Route_Store : In Route_Stores.Map;
                           Track_List : in Track_Lists.List) is

      -- Reports inroute tracks required clear.

      use track_lists;

   begin -- Tracks_Clear
      Put_Line (Output_File, "Aspect requires:-");
      Put (Output_File, "Tracks Clear:");
      if Route_Store (Route).Route_Class /= Shunt then
         for T in Iterate (Track_List) loop
            if T /= Last (Track_List) or else
              Route_Store (Route).Route_Class /= Call_On then
               Put (Output_File, " " & Track_List (T));
            end if; -- T /= Last (Track_List)
         end loop; -- T in Iterate (Track_List)
      end if; -- Route_Store (Route).Class /= Shunt
      New_Line (Output_File);
      Put_Line (Output_File, Dotted_Line);
   end Tracks_Clear;

   procedure Points_Detection (Output_File : in out File_Type;
                               Point_List : in Point_Lists.List) is

      use Point_Lists;
      use Point_End_Sets;

   begin -- Points_Detection
      for Lie in Point_Lies range N .. R loop
         Put (Output_File, "Points " & Lie'Img & ':');
         for P in Iterate (Point_List) loop
            if Lie = Point_List (P).Required_Lie then
               If Length (Point_List (P).In_Route_Ends) = 1 then
                  Put (Output_File, Point_List (P).Point_Number'img);
               else
                  for E in Iterate (Point_List (P).In_Route_Ends) loop
                     Put (Output_File, Point_List (P).Point_Number'img &
                            Element (E));
                  end loop; -- E in Iterate (Point_List (P).In_Route_Ends)
               end if; -- Length (Point_List (P).In_Route_Ends) = 1
            elsif Point_List (P).Required_Lie = Undefined then
               raise Program_Error with "Required lie of" &
                 Point_List (P).Point_Number'img & " not resolved";
            end if; -- Lie = Point_List (P).Required_Lie
         end loop; -- P in Iterate (Point_List)
         New_Line (Output_File);
         if Lie = N then
            Put_Line (Output_File, Dotted_Line);
         else
            Put_Line (Output_File, Solid_Line);
         end if; -- Lie = N
      end loop; -- Lie in Point_Lies range N .. R
   end Points_Detection;

   procedure Route_Holding_Points (Output_File : in out File_Type;
                                   Track_List : in Track_Lists.List;
                                   Point_List : in Point_Lists.List) is

      use Track_Lists;
      use Point_Lists;

      Tc : Track_Lists.Cursor;

   begin -- Route_Holding_Points
      Put_Line (Output_File, "Points, route held by tracks occupied:-");
      for P in Iterate (Point_List) loop
         Put (Output_File, Point_List (P).Point_Number'img & " " &
                Point_List (P).Required_Lie'Img & " :");
         Tc := First (Track_List);
         loop -- until last holding track
            Put (Output_File, " " & Track_List (Tc));
            exit when Track_List (Tc) = Point_List (P).Last_Holding_Track;
            Next (Tc);
         end loop; -- until last holding track
         New_Line (Output_File);
      end loop; -- P in Iterate (Point_List)
      Put_Line (Output_File, Solid_Line);
   end Route_Holding_Points;

   procedure Conflicting_Routes (Output_File : in out File_Type;
                                 Sub_Route_List : in Sub_Route_Lists.List;
                                 Conflict_Map : in Conflict_Maps.Map;
                                 Track_Store : in Track_Stores.List;
                                 Track_Dictionary : in Track_Dictionaries.Map;
                                 Route_Store : in Route_Stores.Map;
                                 Signal_Store : in Signal_Stores.Map) is
      use Sub_Route_Lists;
      use Route_Sets;
      use Conflict_Maps;
      use Track_Lists;

      Function Opposite (Sub_Route : in Sub_Routes) return Sub_Routes is
         ((Sub_Route.Track_Name, Sub_Route.Exit_End, Sub_Route.Entrance_End));

      function Locked_Out_by_Lie (Current : in Sub_Routes;
                                  Track_List : in Track_Lists.List;
                                  Opposing_Route : in Route_Names;
                                  Route_Store : in Route_Stores.Map;
                                  Signal_Store : in Signal_Stores.Map)
                                  return Boolean is
         use Route_Stores;
         use Signal_Stores;

         Opposing_Signal : Signal_Numbers renames
           Route_Store (Opposing_Route).Entrance_Signal;
         Opposing_Key : Track_Keys renames
           Signal_Store (Opposing_Signal).Replacement_Track;

      begin -- Locked_Out_by_Lie
         return Current.Track_Name /= Last_Element (Track_List)
              and then not (Current.Track_Name = Opposing_Key.Track_Name and
                           Current.Exit_End = Opposing_Key.Track_End);
      end Locked_Out_by_Lie;

      package Locked_Lists is new
        Ada.Containers.Ordered_Maps (Route_Names, Sub_Routes);
      use Locked_Lists;

      procedure Diamond_Conflict (Current_In : in Sub_Routes;
                                  Sub_Route_List : in Sub_Route_Lists.List;
                                  Track_Store : in Track_Stores.List;
                                  Track_Dictionary : in Track_Dictionaries.Map;
                                  Conflict_Map : in Conflict_Maps.Map;
                                  Locked_List : in out Locked_Lists.Map) is

         -- Only Diamonds have to be considered, routes that cross at
         -- Switch_Diamonds will be locked by points lie of the diamond.

         function Index (Current : in Sub_Routes;
                         Sub_Route_List : in Sub_Route_Lists.List)
                         return Positive is

            -- Returns the position count of Current into the Sub_Route_List.
            -- Will raise an exception if Current.Track_Name is not contained
            -- in Sub_Route_List.

            Result : Positive := 1;
            Sc : Sub_Route_Lists.Cursor := First (Sub_Route_List);

         begin -- Index
            while Current /= Element (Sc) loop
               Next (Sc);
               Result := Result + 1;
            end loop; -- Sc /= Sub_Route_Lists.No_Element and then ...
            return Result;
         end Index;


         Current : Sub_Routes := Current_In;
         -- Create copy that can be changed if the exit is not clearance.
         Test_Track : Tracks renames
           Track_Store (Track_Dictionary ((Current.Track_Name,
                        Current.Entrance_End)));
         Cross_1, Cross_2 : Sub_Routes;
         E : Diamond_End_Indices;

      begin -- Diamond_Conflict
         if Test_Track.Track_Type = Diamond then
            if (Test_Track.Diamond_End_Array (Left_Straight).This_End =
                  Current.Entrance_End and
                    Test_Track.Diamond_End_Array (Right_Straight).This_End =
                  Current.Exit_End) or
              (Test_Track.Diamond_End_Array (Left_Straight).This_End =
                  Current.Exit_End and
                    Test_Track.Diamond_End_Array (Right_Straight).This_End =
                   Current.Entrance_End) then
               -- conflict on Cross
               Cross_1 :=
                 (Current.Track_Name,
                  Test_Track.Diamond_End_Array (Left_Cross).This_End,
                  Test_Track.Diamond_End_Array (Right_Cross).This_End);
               Cross_2 :=
                 (Current.Track_Name,
                  Test_Track.Diamond_End_Array (Right_Cross).This_End,
                  Test_Track.Diamond_End_Array (Left_Cross).This_End);
            elsif (Test_Track.Diamond_End_Array (Left_Cross).This_End =
                  Current.Entrance_End and
                    Test_Track.Diamond_End_Array (Right_Cross).This_End =
                  Current.Exit_End) or
              (Test_Track.Diamond_End_Array (Left_Cross).This_End =
                  Current.Exit_End and
                    Test_Track.Diamond_End_Array (Right_Cross).This_End =
                   Current.Entrance_End) then
               -- conflict on Straight
               Cross_1 :=
                 (Current.Track_Name,
                  Test_Track.Diamond_End_Array (Left_Straight).This_End,
                  Test_Track.Diamond_End_Array (Right_Straight).This_End);
               Cross_2 :=
                 (Current.Track_Name,
                  Test_Track.Diamond_End_Array (Right_Straight).This_End,
                  Test_Track.Diamond_End_Array (Left_Straight).This_End);
            else
               raise Program_Error with "Subroute " &
                 To_String (Current.Track_Name) & Current.Entrance_End &
                 Current.Exit_End &
                 " does not represent a valid path through a diamond";
            end if; -- (Test_Track.Diamond_End_Array (Left_Straight).This_End ...
            -- Check if the next subroute has to be included due to lack of
            -- clearance. Note the loop exits if the value of Current is
            -- changed, that is, the next subroute becomes the end of locking.
            E := Diamond_End_Indices'First;
            loop -- check one end
               if Test_Track.Diamond_End_Array (E).This_End =
                 Current.Exit_End and
                 not Test_Track.Diamond_End_Array (E).Is_Clear then
                  Current := Sub_Route_List (Next (Find (Sub_Route_List,
                                             Current)));
               end if; -- Test_Track.Diamond_End_Array (E).This_End = ...
               exit when E = Diamond_End_Indices'Last or Current /= Current_in;
               E := Diamond_End_Indices'Succ (E);
            end loop; -- check one end
            for R in Iterate (Conflict_Map (Cross_1)) loop
               if Contains (Locked_List, Element (R)) then
                  -- Routes already have a conflict, required strange geography,
                  -- for example, the diverging ends of a set of points cross
                  -- over each other, Theebine in 1979!
                  if Index (Current, Sub_Route_List) >
                    Index (Locked_List (Element (R)), Sub_Route_List) then
                     Include (Locked_List, Element (R), Current);
                  end if; --  Index (Current, Sub_Route_List) > ...
               else
                  Include (Locked_List, Element (R), Current);
               end if; -- Contains (Locked_List, Element (R))
            end loop; -- R in Iterate (Conflict_Map (Cross_1))
            for R in Iterate (Conflict_Map (Cross_2)) loop
               if Contains (Locked_List, Element (R)) then
                  -- Routes already have a conflict, required strange geography,
                  -- for example, the diverging ends of a set of points cross
                  -- over each other, Theebine in 1979!
                  if Index (Current, Sub_Route_List) >
                    Index (Locked_List (Element (R)), Sub_Route_List) then
                     Include (Locked_List, Element (R), Current);
                  end if; -- Index (Cross_2, Track_list) >
               else
                  Include (Locked_List, Element (R), Current);
               end if; -- Contains (Locked_List, Element (R))
            end loop; -- R in Iterate (Conflict_Map (Cross_2))
         end if; -- Test_Track.Track_Type = Diamond
      end Diamond_Conflict;

      Locked_List : Locked_Lists.Map := Locked_Lists.Empty_Map;
      Track_List : Track_Lists.List := Track_Lists.Empty_List;
      Tc : Track_Lists.Cursor;
      Rc, To_Delete : Locked_Lists.Cursor;

   begin -- Conflicting_Routes
      Put_Line (Output_File,
                "Conflicting Routes, route held by tracks occupied:-");
      -- Find the last track that route holds any opposing route.
      for S in Iterate (Sub_Route_List) loop
         Append (Track_List, Element (S).Track_Name);
         if Contains (Conflict_Map, Opposite (Element (S))) then
            for R in Iterate (Conflict_Map (Opposite (Element (S)))) loop
               include (Locked_List, Element (R), Element (S));
            end loop; -- R in Iterate (Conflict_Map (Opposite (Element (S))))
         end if; -- Contains (Conflict_Map, Opposite (Element (S)))
      end loop; -- S in Iterate (Sub_Route_List)
      Rc := First (Locked_List);
      loop -- one route
         if Locked_Out_by_Lie (Locked_List (Rc), Track_List, Key (Rc),
                               Route_Store, Signal_Store) then
            To_Delete := Rc;
            Next (Rc);
            Delete (Locked_List, To_Delete);
         else
            Next (Rc);
         end if; -- Locked_Out_by_Lie (Locked_List (Rc), Track_List ...
         exit when Rc = Locked_Lists.No_Element;
      end loop; -- one route
      -- Check for conflicts at diamonds.
      for S in Iterate (Sub_Route_List) loop
         Diamond_Conflict (Element (S), Sub_Route_List, Track_Store,
                           Track_Dictionary, Conflict_Map, Locked_List);
      end loop; -- S in Iterate (Sub_Route_List)
      for T in Iterate (Track_List) loop
         for R in Iterate (Locked_List) loop
            if Locked_List (R).Track_Name = Element (T) then
               Put (Output_File, Key (R) & " :");
               Tc := First (Track_List);
               loop -- until last holding track
                  Put (Output_File, " " & Track_List (Tc));
                  exit when Track_List (Tc) = Locked_List (R).Track_Name;
                  Next (Tc);
               end loop; -- until last holding track
               New_Line (Output_File);
            end if; -- Locked_List (R).Track_Name = Element (T)
         end loop; -- R in Iterate (Locked_List)
      end loop; -- T in Iterate (Track_List)
      Put_Line (Output_File, Solid_Line);
   end Conflicting_Routes;

   Track_Store : Track_Stores.List;
   Signal_Store : Signal_Stores.Map;
   Route_Store : Route_Stores.Map;
   Track_Dictionary : Track_Dictionaries.Map;
   Sub_Route_to_Signal_Map : Sub_Route_to_Signal_Maps.Map;
   Route_Map : Route_Maps.Map;
   Conflict_Map : Conflict_Maps.Map;
   Output_File : File_Type;
   Track_List : Track_Lists.List;
   Point_List : Point_Lists.List;

begin
   Put_Line ("Basic Control Table 20230601");
   Get (Track_Store);
   Get (Signal_Store);
   Get (Route_Store);
   Build (Track_Store, Track_Dictionary);
   Build (Signal_Store, Track_Dictionary, Sub_Route_to_Signal_Map);
   Build (Track_Store, Signal_Store, Route_Store, Track_Dictionary,
          Sub_Route_to_Signal_Map, Route_Map);
   Build (Route_Map, Conflict_Map);
   for R in Iterate (Route_Store) loop
      Create (Output_File, Out_File, Compose (Output_Path, To_String (Key (R)),
              "txt"));
      Header (Output_File, Key (R), Route_Store);
      Build (Route_Map (Key (R)), Track_List);
      Tracks_Clear (Output_File, Key (R), Route_Store, Track_List);
      Build (Track_Store, Track_Dictionary, Route_Map (Key (R)), Point_List);
      Points_Detection (Output_File, Point_List);
      Route_Holding_Points (Output_File, Track_List, Point_List);
      Conflicting_Routes (Output_File, Route_Map (Key (R)), Conflict_Map,
                          Track_Store, Track_Dictionary, Route_Store,
                          Signal_Store);
      Put_Line (Output_File, "End of text for " & Key (R));
      Close (Output_File);
   end loop; -- R in Iterate (Route_Store)
   Put_Line ("Processing Complete");
end Basic_Ct;
