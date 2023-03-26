-- Package to provided type declarations for Basic_CT.

-- Author    : David Haley
-- Created   : 26/03/2023
-- Last Edit : 26/03/2023

with Ada.Containers; use Ada.Containers;

package body CT_Types is

   function "<" (Left, Right : Track_Keys) return Boolean is
     (Left.Track_Name < Right.Track_Name or (Left.Track_Name = Right.Track_Name
      and Left.Track_End < Right.Track_End));

   function "=" (Left, Right : Sub_Route_Lists.Vector) return Boolean is

      use Sub_Route_Lists;

      Result : Boolean := True;

   begin -- "="
      if Length (Left) = Length (Right) then
         for Lc in Iterate (Left) loop
            Result := Result and Element (Lc) = Right (To_Index (Lc));
         end loop; -- Lc in Iterate (Left)
      else
         Result := False;
      end if; -- Length (Left) = Length (Right)
      return Result;
   end "=";

end CT_Types;
