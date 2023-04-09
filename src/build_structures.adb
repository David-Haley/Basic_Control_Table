-- Package to build data structures.

-- Author    : David Haley
-- Created   : 27/03/2023
-- Last Edit : 09/04/2023
-- 20230409 : Track linkage verification added.
-- 20230408 : Context Ada.Strings.Unbounded added.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Exceptions; use Ada.Exceptions;

package body Build_Structures is

   procedure Build (Track_Store : in Track_Stores.Vector;
                    Track_Dictionary : out Track_Dictionaries.Map) is

      -- Builds an ordered list of all track entereces.

      use Track_Stores;
      use Track_Dictionaries;

      procedure Insert (Track_Dictionary : in out Track_Dictionaries.Map;
                        Track_Key : in Track_Keys;
                        Tc : in Track_Stores.Cursor) is

      begin -- Insert
         if Contains (Track_Dictionary, Track_Key) then
            raise Data_Error with "At row " & To_Index (Tc)'Img &
              " the combination of track name " &
              To_String (Track_Key.Track_Name) &
              " track end '" & Track_Key.Track_End & "' is repeated";
         else
            Include (Track_Dictionary, Track_Key, To_Index (Tc));
         end if; -- Insert
      end Insert;

      procedure Verify_Links (Track_Store : in Track_Stores.Vector) is

         package Link_Sets is new Ada.Containers.Ordered_Sets (Track_Keys);
         use Link_Sets;

         procedure Insert (Link_Set : in out Link_Sets.Set;
                           Link, Current_End: in Track_Keys) is

         begin -- Insert
            if Link.Track_Name /= Null_Unbounded_String then
               -- don't include end of world
               if Contains (Link_Set, Link) then
                  raise Data_Error with "Track  " &
                    To_String (Current_End.Track_Name) & " end " &
                    Current_End.Track_End & " duplicate link to " &
                  To_String (Link.Track_Name) & " end " & Link.Track_End;
               else
                  Include (Link_Set, Link);
               end if; -- Contains (Link_Set, Link)
            end if; -- Link.Track_Name /= Null_Unbounded_String
         end Insert;

         Link_Set : Link_Sets.Set := Link_Sets.Empty_Set;
         Current_End, Adjacent_End, Back_Link_Target : Track_Keys;

         function Find_Adjacent (Track_Store : in Track_Stores.Vector;
                                    Track_Dictionary : in Track_Dictionaries.Map;
                                    Link : in Track_Keys) return Track_Keys is

            Track : Tracks := Track_Store (Track_Dictionary (Link));
            Result : Track_Keys := Null_Link; -- default if no match found

         begin -- Find_Adjacent
            case Track.Track_Type is
               when Plain =>
                  if Link.Track_End = Track.Left_End then
                     Result := Track.Adjacent_Left;
                  elsif Link.Track_End = Track.Right_End then
                     Result := Track.Adjacent_Right;
                  end if; -- Link.Track_End = Track.Left_End
               when Points =>
                  for E in Point_End_Indices loop
                     if Link.Track_End = Track.Point_End_Array (E).This_End then
                        Result := Track.Point_End_Array (E).Adjacent;
                     end if; -- Link.Track_End = ...
                  end loop; -- E in Point_End_Indices
               when Diamond =>
                  for E in Diamond_End_Indices loop
                     if Link.Track_End =
                       Track.Diamond_End_Array (E).This_End then
                        Result := Track.Diamond_End_Array (E).Adjacent;
                     end if; -- Link.Track_End ...
                  end loop; -- E in Diamond_End_Indices
               when Switch_Diamond =>
                  for E in Diamond_End_Indices loop
                     if Link.Track_End =
                       Track.Switch_Diamond_End_Array (E).This_End then
                        Result := Track.Switch_Diamond_End_Array (E).Adjacent;
                     end if; -- Link.Track_End ...
                  end loop; -- E in Diamond_End_Indices
            end case; -- Track.Track_Type
            return Result;
         end Find_Adjacent;

      begin -- Verify_Links
         Put_Line ("Verifying adjacent track links are unique.");
         for T in Iterate (Track_Store) loop
            Current_End.Track_Name := Track_Store (T).Track_Name;
            case Track_Store (T).Track_Type is
               when Plain =>
                  Current_End.Track_End := Track_Store (T).Left_End;
                  Insert (Link_Set, Track_Store (T).Adjacent_Left, Current_End);
                  Current_End.Track_End := Track_Store (T).Right_End;
                  Insert (Link_Set, Track_Store (T).Adjacent_Right,
                          Current_End);
               when Points =>
                  for E in Point_End_Indices loop
                     Current_End.Track_End :=
                       Track_Store (T).Point_End_Array (E).This_End;
                     Insert (Link_Set,
                             Track_Store (T).Point_End_Array (E).Adjacent,
                             Current_End);
                  end loop; -- E in Point_End_Indices
               when Diamond =>
                  for E in Diamond_End_Indices loop
                     Current_End.Track_End :=
                       Track_Store (T).Diamond_End_Array (E).This_End;
                     Insert (Link_Set,
                             Track_Store (T).Diamond_End_Array (E).Adjacent,
                             Current_End);
                  end loop; -- E in Diamond_End_Indices
               when Switch_Diamond =>
                  for E in Diamond_End_Indices loop
                     Current_End.Track_End :=
                       Track_Store (T).Switch_Diamond_End_Array (E).This_End;
                     Insert (Link_Set,
                             Track_Store (T).Switch_Diamond_End_Array (E).
                               Adjacent, Current_End);
                  end loop; -- E in Diamond_End_Indices
            end case; -- Track_Store (T).Track_Type
         end loop; -- T in Iterate (Track_Store)
         Put_Line ("Verification complete," & Length (Link_Set)'Img &
                     " links found.");
         Put_Line ("Verifying links are consistent in both directions.");
         for L in Iterate (Link_Set) loop
            -- Adjoining track pairs are effectively checked twice, deleting
            -- verified links would preclude simple iteration schemes.
            Adjacent_End := Find_Adjacent (Track_Store, Track_Dictionary,
                                           Element (L));
            Back_Link_Target := Find_Adjacent (Track_Store, Track_Dictionary,
                                          Adjacent_End);
            if Element (L) /= Back_Link_Target then
               raise Data_Error with "Invalid linkage track adjacent to track "
                 & To_String (Element (L).Track_Name) & " end " &
                 Element (L).Track_End & " links to track " &
                 To_String (Adjacent_End.Track_Name) & " end " &
                 Adjacent_End.Track_End & " linked back to " &
                 To_String (Back_Link_Target.Track_Name) & " end " &
                 Back_Link_Target.Track_End;
            end if; -- Element (L) /= Back_Link_Target
         end loop; -- L in Iterate (Link_Set)
         Put_Line ("Verification of link consistency complete");
         Clear (Link_Set);
      end Verify_Links;

      Track_Key : Track_Keys;
      Errors_Detected : Boolean := False;

   begin -- Build
      Put_Line ("Building Track_Dictionary");
      Clear (Track_Dictionary);
      for Tc in Iterate (Track_Store) loop
         begin -- Include exception block
            Track_Key.Track_Name := Track_Store (Tc).Track_Name;
            case Track_Store (Tc).Track_Type is
            when Plain =>
               Track_Key.Track_End := Track_Store (Tc).Left_End;
               Insert (Track_Dictionary, Track_Key, Tc);
               Track_Key.Track_End := Track_Store (Tc).Right_End;
               Insert (Track_Dictionary, Track_Key, Tc);
            when Points =>
               for E in Point_End_Indices loop
                  Track_Key.Track_End :=
                    Track_Store (Tc).Point_End_Array (E).This_End;
                  Insert (Track_Dictionary, Track_Key, Tc);
               end loop; -- E in Point_End_Indices
            when Diamond =>
               for E in Diamond_End_Indices loop
                  Track_Key.Track_End :=
                    Track_Store (Tc).Diamond_End_Array (E).This_End;
                  Insert (Track_Dictionary, Track_Key, Tc);
               end loop; -- E in Diamond_End_Indices
            when Switch_Diamond =>
               for E in Diamond_End_Indices loop
                  Track_Key.Track_End :=
                    Track_Store (Tc).Switch_Diamond_End_Array (E).
                    This_End;
                  Insert (Track_Dictionary, Track_Key, Tc);
               end loop; -- E in Diamond_End_Indices
            end case; -- Track_Store (T).Track_Type
         exception
            when E : others =>
               Put_Line (Exception_Name (E) & " - " & Exception_Message (E));
               Errors_Detected := True;
         end; -- Include exception block
      end loop; -- Tc in Iterate (Track_Store)
      begin -- Verification block
         Verify_Links (Track_Store);
      exception
         when E : others =>
            Put_Line (Exception_Name (E) & " - " & Exception_Message (E));
            Errors_Detected := True;
      end; -- Verification block
      if Errors_Detected then
         raise Data_Error with "Errors detected building Track_Dictionary";
      end if; -- Errors_Detected
   end Build;

   procedure Build (Signal_Store : in Signal_Stores.Map;
                    Track_Dictionary : in Track_Dictionaries.Map;
                    Sub_Route_to_Signal_Map : out Sub_Route_to_Signal_Maps.Map)
   is

      -- Builds a list of signals indexed by their replacement tracks;

      use Signal_Stores;
      use Track_Dictionaries;
      use Sub_Route_to_Signal_Maps;

      Track_Key : Track_Keys;
      Errors_Detected : Boolean := False;

   begin -- Build
      Clear (Sub_Route_to_Signal_Map);
      Put_Line ("Building Sub_Route_to_Signal_Map");
      for Sc in Iterate (Signal_Store) loop
         begin -- Include exception block
            Track_Key := Signal_Store (Sc).Replacement_Track;
            if Contains (Sub_Route_to_Signal_Map, Track_Key) then
               raise Data_Error with "Duplicate signal" & Key (Sc)'img & " at "
                 & Signal_Store (Sc).Replacement_Track.Track_End & " end of " &
                 To_String (Signal_Store (Sc).Replacement_Track.Track_Name) &
                 " track";
            else
               include (Sub_Route_to_Signal_Map, Track_Key, Key (Sc));
            end if; -- Contains (Sub_Route_to_Signal_Map, Track_Key)
         exception
            when E : others =>
               Put_Line (Exception_Name (E) & " - " & Exception_Message (E));
               Errors_Detected := True;
         end; -- Include exception block
      end loop; -- Sc in Iterate (Signal_Store)
      if Errors_Detected then
         raise Data_Error with
           "Errors detected building Sub_Route_to_Signal_Map";
      end if; -- Errors_Detected
   end Build;

   procedure Build (Track_Store : in Track_Stores.Vector;
                    Signal_Store : in Signal_Stores.Map;
                    Route_Store : in Route_Stores.Map;
                    Track_Dictionary : in Track_Dictionaries.Map;
                    Sub_Route_to_Signal_Map : in Sub_Route_to_Signal_Maps.Map;
                    Route_Map : out Route_Maps.Map) is

   -- Builds a list of all the in-route tracks forming a route starting from
   -- the entrance signal and ending with the track before the exit signal
   -- replacement track.

      use Track_Stores;
      use Signal_Stores;
      use Route_Stores;
      use Track_Dictionaries;
      use Sub_Route_to_Signal_Maps;
      use Sub_Route_Lists;
      use Route_Maps;

      procedure Validate_Routes (Signal_Store : in Signal_Stores.Map;
                                 Route_Store : in Route_Stores.Map) is

         -- Checks that Entrance signal and Exit Signal both exist and that the
         -- signals are appropriate for the class of route.

      begin -- Validate_Routes
         Put_Line ("Validating routes (no route trace)");
         for R in Iterate (Route_Store) loop
            if not Contains (Signal_Store, Route_Store (R).Entrance_Signal) then
               raise Data_Error with "Route " & To_String (Key (R)) &
                 " entrance signal" & Route_Store (R).Entrance_Signal'Img &
                 " not defined.";
            elsif Route_Store (R).Route_Class in Main_Route_Classes and
              not Signal_Store (Route_Store (R).Entrance_Signal).Is_Main then
               raise Data_Error with "Entrance signal" &
                 Route_Store (R).Entrance_Signal'Img & " of route " &
                 To_String (Key (R)) & "is not a main signal.";
            end if; -- not Contains (Signal_Store, Route_Store (R) ...
            if not Contains (Signal_Store, Route_Store (R).Exit_Signal) then
               raise Data_Error with "Route " & To_String (Key (R)) &
                 " exit signal" & Route_Store (R).Exit_Signal'Img &
                 " not defined.";
            elsif Route_Store (R).Route_Class in Main_Route_Classes and
              not Signal_Store (Route_Store (R).Exit_Signal).Is_Main then
               raise Data_Error with "Exit signal" &
                 Route_Store (R).Exit_Signal'Img & " of route " &
                 To_String (Key (R)) & "is not a main signal.";
            end if; -- not Contains (Signal_Store, Route_Store (R).Exit_Signal
         end loop; -- R in Iterate (Route_Store)
        Put_Line ("Finished validating routes");
      end Validate_Routes;

      function Find_Exit (Track_Store : in Track_Stores.Vector;
                          Track_Dictionary : in Track_Dictionaries.Map;
                          Track_Key : in Track_Keys)
                             return Sub_Route_Lists.Vector is

         Track : Tracks := Track_Store (Track_Dictionary (Track_Key));
         Result : Sub_Route_Lists.Vector := Sub_Route_Lists.Empty_Vector;
         Sub_Route : Sub_Routes;

      begin -- Find_Exit
         Sub_Route.Track_Name := Track_Key.Track_Name;
         Sub_Route.Entrance_End := Track_Key.Track_End;
         case Track.Track_Type is
            when Plain =>
               if Track_Key.Track_End = Track.Left_End then
                  Sub_Route.Exit_End := Track.Right_End;
                  Append (Result, Sub_Route);
               elsif Track_Key.Track_End = Track.Right_End then
                  Sub_Route.Exit_End := Track.Left_End;
                  Append (Result, Sub_Route);
               else
                  raise Data_Error with "Error in linkage " &
                    To_String (Track.Track_Name) & " doesn't have end " &
                    Track_Key.Track_End;
               end if; -- Track_Key.Track_End = ...
            when Points =>
               if Track_Key.Track_End =
                 Track.Point_End_Array (Facing).This_End then
                  Sub_Route.Exit_End :=
                    Track.Point_End_Array (Straight).This_End;
                  Append (Result, Sub_Route);
                  Sub_Route.Exit_End :=
                    Track.Point_End_Array (Divergent).This_End;
                  Append (Result, Sub_Route);
               elsif Track_Key.Track_End =
                 Track.Point_End_Array (Straight).This_End or
                 Track_Key.Track_End =
                   Track.Point_End_Array (Divergent).This_End then
                  Sub_Route.Exit_End :=
                    Track.Point_End_Array (Facing).This_End;
                  Append (Result, Sub_Route);
               else
                  raise Data_Error with "Error in linkage " &
                    To_String (Track.Track_Name) & " doesn't have end " &
                    Track_Key.Track_End;
               end if; -- Track_Key.Track_End = ...
            when Diamond =>
               if Track_Key.Track_End =
                 Track.Diamond_End_Array (Left_Straight).This_End then
                  Sub_Route.Exit_End :=
                    Track.Diamond_End_Array (Right_Straight).This_End;
                  Append (Result, Sub_Route);
               elsif Track_Key.Track_End =
                 Track.Diamond_End_Array (Left_Cross).This_End then
                  Sub_Route.Exit_End :=
                    Track.Diamond_End_Array (Right_Cross).This_End;
                  Append (Result, Sub_Route);
               elsif Track_Key.Track_End =
                 Track.Diamond_End_Array (Right_Straight).This_End then
                  Sub_Route.Exit_End :=
                    Track.Diamond_End_Array (Left_Straight).This_End;
                  Append (Result, Sub_Route);
               elsif Track_Key.Track_End =
                 Track.Diamond_End_Array (Right_Cross).This_End then
                  Sub_Route.Exit_End :=
                    Track.Diamond_End_Array (Left_Cross).This_End;
                  Append (Result, Sub_Route);
               else
                  raise Data_Error with "Error in linkage " &
                    To_String (Track.Track_Name) & " doesn't have end " &
                    Track_Key.Track_End;
               end if; -- Track_Key.Track_End = ...
            when Switch_Diamond =>
               if Track_Key.Track_End =
                 Track.Switch_Diamond_End_Array (Left_Straight).This_End then
                  Sub_Route.Exit_End :=
                    Track.Switch_Diamond_End_Array (Right_Straight).This_End;
                  Append (Result, Sub_Route);
               elsif Track_Key.Track_End =
                 Track.Switch_Diamond_End_Array (Left_Cross).This_End then
                  Sub_Route.Exit_End :=
                    Track.Switch_Diamond_End_Array (Right_Cross).This_End;
                  Append (Result, Sub_Route);
               elsif Track_Key.Track_End =
                 Track.Switch_Diamond_End_Array (Right_Straight).This_End then
                  Sub_Route.Exit_End :=
                    Track.Switch_Diamond_End_Array (Left_Straight).This_End;
                  Append (Result, Sub_Route);
               elsif Track_Key.Track_End =
                 Track.Switch_Diamond_End_Array (Right_Cross).This_End then
                  Sub_Route.Exit_End :=
                    Track.Switch_Diamond_End_Array (Left_Cross).This_End;
                  Append (Result, Sub_Route);
               else
                  raise Data_Error with "Error in linkage " &
                    To_String (Track.Track_Name) & " doesn't have end " &
                    Track_Key.Track_End;
               end if; -- Track_Key.Track_End = ...
         end case; -- Track.Track_Type
         return Result;
      end Find_Exit;

      procedure Find_Route (Track_Store : in Track_Stores.Vector;
                            Track_Dictionary : in Track_Dictionaries.Map;
                            Route_End : in Track_Keys;
                            Is_Main : in Boolean;
                            Found : in out Boolean;
                            Sub_Route_List : in out Sub_Route_Lists.Vector) is

         function Find_Next_Key (Track_Store : in Track_Stores.Vector;
                                 Track_Dictionary : in Track_Dictionaries.Map;
                                 Sub_Route : in Sub_Routes) return Track_Keys is

            Track : Tracks :=
              Track_Store (Track_Dictionary ((Sub_Route.Track_Name,
                           Sub_Route.Exit_End)));
            Next_Track_Key : Track_Keys;

         begin -- Find_Next_Key
            case Track.Track_Type is
               when Plain =>
                  if Sub_Route.Exit_End = Track.Left_End then
                     Next_Track_Key := Track.Adjacent_Left;
                  elsif Sub_Route.Exit_End = Track.Right_End then
                     Next_Track_Key := Track.Adjacent_Right;
                  else
                     raise Data_Error with "Error in linkage " &
                       To_String (Track.Track_Name) & " doesn't have end " &
                       Sub_Route.Exit_End;
                  end if; -- Sub_Route.Exit_End = ...
               when Points =>
                  if Sub_Route.Exit_End =
                    Track.Point_End_Array (Facing).This_End then
                     Next_Track_Key := Track.Point_End_Array (Facing).Adjacent;
                  elsif Sub_Route.Exit_End =
                    Track.Point_End_Array (Straight).This_End then
                     Next_Track_Key :=
                       Track.Point_End_Array (Straight).Adjacent;
                  elsif Sub_Route.Exit_End =
                    Track.Point_End_Array (Divergent).This_End then
                     Next_Track_Key :=
                       Track.Point_End_Array (Divergent).Adjacent;
                  else
                     raise Data_Error with "Error in linkage " &
                       To_String (Track.Track_Name) & " doesn't have end " &
                       Sub_Route.Exit_End;
                  end if; -- Sub_Route.Exit_End = ...
               when Diamond =>
                  if Sub_Route.Exit_End =
                    Track.Diamond_End_Array (Left_Straight).This_End then
                     Next_Track_Key :=
                       Track.Diamond_End_Array (Left_Straight).Adjacent;
                  elsif Sub_Route.Exit_End =
                    Track.Diamond_End_Array (Left_Cross).This_End then
                     Next_Track_Key :=
                       Track.Diamond_End_Array (Left_Cross).Adjacent;
                  elsif Sub_Route.Exit_End =
                    Track.Diamond_End_Array (Right_Straight).This_End then
                     Next_Track_Key :=
                       Track.Diamond_End_Array (Right_Straight).Adjacent;
                  elsif Sub_Route.Exit_End =
                    Track.Diamond_End_Array (Right_Cross).This_End then
                     Next_Track_Key :=
                       Track.Diamond_End_Array (Right_Cross).Adjacent;
                  else
                     raise Data_Error with "Error in linkage " &
                       To_String (Track.Track_Name) & " doesn't have end " &
                       Sub_Route.Exit_End;
                  end if; -- Sub_Route.Exit_End = ...
               when Switch_Diamond =>
                  if Sub_Route.Exit_End =
                    Track.Switch_Diamond_End_Array (Left_Straight).This_End then
                     Next_Track_Key :=
                       Track.Switch_Diamond_End_Array (Left_Straight).Adjacent;
                  elsif Sub_Route.Exit_End =
                    Track.Switch_Diamond_End_Array (Left_Cross).This_End then
                     Next_Track_Key :=
                       Track.Switch_Diamond_End_Array (Left_Cross).Adjacent;
                  elsif Sub_Route.Exit_End =
                    Track.Switch_Diamond_End_Array (Right_Straight).This_End
                  then
                     Next_Track_Key :=
                       Track.Switch_Diamond_End_Array (Right_Straight).
                       Adjacent;
                  elsif Sub_Route.Exit_End =
                    Track.Switch_Diamond_End_Array (Right_Cross).This_End then
                     Next_Track_Key :=
                       Track.Switch_Diamond_End_Array (Right_Cross).Adjacent;
                  else
                     raise Data_Error with "Error in linkage " &
                       To_String (Track.Track_Name) & " doesn't have end " &
                       Sub_Route.Exit_End;
                  end if; -- Sub_Route.Exit_End = ...
            end case; -- Track.Track_Type
            return Next_Track_Key;
         end Find_Next_Key;

         Current_Track_Key, Next_Track_Key : Track_Keys;
         Wrong_Exit : Boolean;
         Test_List : Sub_Route_Lists.Vector;
         Sc : Sub_Route_Lists.Cursor;

      begin -- Find_Route
         Current_Track_Key := (Last_Element (Sub_Route_List).Track_Name,
                               Last_Element (Sub_Route_List).Entrance_End);
         Found := Current_Track_Key = Route_End;
         Wrong_Exit := not Found -- intended exit found
           and then
             Contains (Sub_Route_to_Signal_Map,Current_Track_Key)
           -- signal exists
           and then
             (Signal_Store (Sub_Route_to_Signal_Map (Current_Track_Key)).
                Is_Main or not Is_Main)
           and then
           Length (Sub_Route_List) > 1; -- not start of search
         -- Main or Shunt and Shunt route True if an end of route is found which
         -- is not the intended exit, terminates search;
         if Found then
            Delete_Last (Sub_Route_List); -- Last Sub_Route is replacement track
         elsif not Wrong_Exit then
            -- Continue search
            Next_Track_Key := Find_Next_Key (Track_Store, Track_Dictionary,
                                             Last_Element (Sub_Route_List));
            if Contains (Track_Dictionary, Next_Track_Key) then
               -- Only continue search if not "end of world" e.g. trap point
               -- or limit of data.
               Test_List := Find_Exit (Track_Store, Track_Dictionary,
                                       Next_Track_Key);
               Sc := First (Test_List);
               while not Found and Sc /= Sub_Route_Lists.No_Element loop
                  Append (Sub_Route_List, Test_List (Sc));
                  Find_Route (Track_Store, Track_Dictionary, Route_End, Is_Main,
                              Found, Sub_Route_List);
                  if not Found then
                     Delete_Last (Sub_Route_List);
                  end if; -- not Found
                  Next (Sc);
               end loop; -- not Found and Sc /= Sub_Route_Lists.No_Element
            end if; -- Contains (Track_Dictionary, Next_Track_Key)
         end if; -- not Found
      end Find_Route;

      Route_Entrance : Track_Keys;
      Test_List, Sub_Route_List : Sub_Route_Lists.Vector;
      Tc : Sub_Route_Lists.Cursor;
      Is_Main : Boolean;
      Found : Boolean := False;

   begin -- Build
      Put_Line ("Building Route_Map");
      Validate_Routes (Signal_Store, Route_Store);
      Clear (Route_Map);
      for R in Iterate (Route_Store) loop
         Clear (Test_List);
         Route_Entrance :=
           Signal_Store (Route_Store (R).Entrance_Signal).Replacement_Track;
         Test_List := Find_Exit (Track_Store, Track_Dictionary, Route_Entrance);
         Is_Main := Route_Store (R).Route_Class in Main_Route_Classes;
         Tc := First (Test_List);
         loop -- through exits of entrance track
            Clear (Sub_Route_List);
            Append (Sub_Route_List, Test_List (Tc));
            Next (Tc);
            Find_Route (Track_Store, Track_Dictionary,
                        Signal_Store (Route_Store (R).Exit_Signal).
                          Replacement_Track,
                        Is_Main, Found, Sub_Route_List);
            exit when Found or Tc = Sub_Route_Lists.No_Element;
         end loop; -- through exits of entrance track
         if Found then
            include (Route_Map, Key (R), Sub_Route_List);
         else
            raise Data_Error with "Unable to resolve route " &
              To_String (Key (R));
         end if; -- Found
      end loop; -- R in Iterate (Route_Store)
   end Build;

end Build_Structures;
