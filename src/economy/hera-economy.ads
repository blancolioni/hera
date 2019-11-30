with Hera.Handles.Commodity;
with Hera.Handles.Economic_Activity;
with Hera.Handles.Economic_Sector;
with Hera.Handles.Economy;

package Hera.Economy is

   subtype Economic_Sector_Handle is
     Hera.Handles.Economic_Sector.Economic_Sector_Handle;

   subtype Economic_Activity_Handle is
     Hera.Handles.Economic_Activity.Economic_Activity_Handle;

   subtype Economy_Handle is
     Hera.Handles.Economy.Economy_Handle;

   function Base_Pressure
     (Economy   : Economy_Handle'Class;
      Commodity : Hera.Handles.Commodity.Commodity_Handle'Class)
      return Signed_Unit_Real;

end Hera.Economy;
