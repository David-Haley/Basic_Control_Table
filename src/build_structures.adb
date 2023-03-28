-- Package to build data structures.

-- Author    : David Haley
-- Created   : 27/03/2023
-- Last Edit : 28/03/2023

with Ada.Text_IO; use Ada.Text_IO;
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
               Track_Key.Track_End :=
                 Track_Store (Tc).Point_End_Array (Facing).This_End;
               Insert (Track_Dictionary, Track_Key, Tc);
               Track_Key.Track_End :=
                 Track_Store (Tc).Point_End_Array (Straight).This_End;
               Insert (Track_Dictionary, Track_Key, Tc);
               Track_Key.Track_End :=
                 Track_Store (Tc).Point_End_Array (Divergent).This_End;
               Insert (Track_Dictionary, Track_Key, Tc);
            when Diamond =>
               Track_Key.Track_End :=
                 Track_Store (Tc).Diamond_End_Array (Left_Straight).This_End;
               Insert (Track_Dictionary, Track_Key, Tc);
               Track_Key.Track_End :=
                 Track_Store (Tc).Diamond_End_Array (Right_Straight).This_End;
               Insert (Track_Dictionary, Track_Key, Tc);
               Track_Key.Track_End :=
                 Track_Store (Tc).Diamond_End_Array (Left_Cross).This_End;
               Insert (Track_Dictionary, Track_Key, Tc);
               Track_Key.Track_End :=
                 Track_Store (Tc).Diamond_End_Array (Right_Cross).This_End;
               Insert (Track_Dictionary, Track_Key, Tc);
            when Switch_Diamond =>
               Track_Key.Track_End :=
                 Track_Store (Tc).Switch_Diamond_End_Array (Left_Straight).
                 This_End;
               Insert (Track_Dictionary, Track_Key, Tc);
               Track_Key.Track_End :=
                 Track_Store (Tc).Switch_Diamond_End_Array (Right_Straight).
                 This_End;
               Insert (Track_Dictionary, Track_Key, Tc);
               Track_Key.Track_End :=
                 Track_Store (Tc).Switch_Diamond_End_Array (Left_Cross).
                 This_End;
               Insert (Track_Dictionary, Track_Key, Tc);
               Track_Key.Track_End :=
                 Track_Store (Tc).Switch_Diamond_End_Array (Right_Cross).
                 This_End;
               Insert (Track_Dictionary, Track_Key, Tc);
            end case; -- Track_Store (T).Track_Type
         exception
            when E : others =>
               Put_Line (Exception_Name (E) & " - " & Exception_Message (E));
               Errors_Detected := True;
         end; -- Include exception block
      end loop; -- Tc in Iterate (Track_Store)
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
      Put_Line (" Building Sub_Route_to_Signal_Map");
      for Sc in Iterate (Signal_Store) loop
         begin -- Include exception block
            Track_Key := (Signal_Store (Sc).Replacement_Track,
                          Signal_Store (Sc).Entrance_End);
            if Contains (Sub_Route_to_Signal_Map, Track_Key) then
               raise Data_Error with "Duplicate signal" &
                 Key (Sc)'img & " at " & Signal_Store (Sc).Entrance_End &
                 " end of " & To_String (Signal_Store (Sc).Replacement_Track) &
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

      procedure Find_Route (Track_Store : in Track_Stores.Vector;
                            Track_Dictionary : in Track_Dictionaries.Map;
                            Route_End : in Track_Keys;
                            Is_Main : in Boolean;
                            Found : in out Boolean;
                            Sub_Route_List : in out Sub_Route_Lists.Vector) is

         Test_Sub_Route : Sub_Routes;
         Current_Track, Next_Track : Track_Indices;
         Next_Track_Key : Track_Keys;

      begin -- Find_Route
         Found :=
           Last_Element (Sub_Route_List).Track_Name = Route_End.Track_Name and
           Last_Element (Sub_Route_List).Exit_End = Route_End.Track_End;
         if not Found then
            Current_Track :=
              Track_Dictionary ((Last_Element (Sub_Route_List).Track_Name,
                                Last_Element (Sub_Route_List).Entrance_End));
            case Track_Store (Current_Track).Track_Type is
               when Plain =>
                  if Track_Store (Current_Track).Left_End =
                    Last_Element (Sub_Route_List).Exit_End then
                     Next_Track_Key :=
                       (Track_Store (Current_Track).Adjacent_Left_Track,
                        Track_Store (Current_Track).Adjacent_Left_End);
                  elsif Track_Store (Current_Track).Right_End =
                    Last_Element (Sub_Route_List).Exit_End then
                     Next_Track_Key :=
                       (Track_Store (Current_Track).Adjacent_Right_Track,
                        Track_Store (Current_Track).Adjacent_Right_End);
                  else
                     raise Data_Error with "Error in linkage " &
                       To_String (Track_Store (Current_Track).Track_Name) &
                       " doesn't have end " &
                       Last_Element (Sub_Route_List).Exit_End;
                  end if; -- Track_Store (Current_Track).Adjacent_Left_End ...
               when Points =>
                  null;
               when Diamond =>
                  null;
               when Switch_Diamond =>
                  null;
            end case; --
            if Contains (Track_Dictionary, Next_Track_Key) then
               Next_Track := Track_Dictionary (Next_Track_Key);
               Test_Sub_Route.Track_Name := Next_Track_Key.Track_Name;
               Test_Sub_Route.Entrance_End := Next_Track_Key.Track_End;

            end if; -- Contains (Track_Dictionary, Next_Track_Key)
         end if; -- not Found
      end Find_Route;

   begin -- Build
      Clear (Route_Map);
   end Build;

end Build_Structures;
