-- Package to read input data for Basic_CT.

-- Author    : David Haley
-- Created   : 25/03/2023
-- Last Edit : 26/03/2023

with CT_Types; use CT_Types;

package Get_Data is

   procedure Get (Track_Store : out Track_Stores.Vector);

   procedure Get (Signal_Store : out Signal_Stores.Map);

   procedure Get (Route_Store : out Route_Stores.Map);

end Get_Data;
