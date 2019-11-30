private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

private with WL.Random.Names;

with Tropos;

package Hera.Scenarios is

   type Hera_Scenario is tagged private;

   function Open_Scenario (Name : String) return Hera_Scenario;

   procedure Load
     (Scenario       : Hera_Scenario;
      Directory_Name : String;
      Extension      : String;
      Loader         : not null access
        procedure (Config : Tropos.Configuration));

   function Directory_Path
     (Scenario       : Hera_Scenario;
      Directory_Name : String)
      return String;

   function File_Path
     (Scenario  : Hera_Scenario;
      File_Name : String)
      return String;

   function File_Path
     (Scenario       : Hera_Scenario;
      Directory_Name : String;
      File_Name      : String)
      return String;

   function Core_System_Count
     (Scenario : Hera_Scenario)
      return Natural;

   function Non_Core_System_Count
     (Scenario : Hera_Scenario)
      return Natural;

   function Initial_System_Count
     (Scenario : Hera_Scenario)
      return Natural;

   procedure Galaxy_Axes
     (Scenario : Hera_Scenario;
      X, Y, Z  : out Non_Negative_Real);

   function Current_Scenario
     return access constant Hera_Scenario'Class;

private

   package Path_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   type Hera_Scenario is tagged
      record
         Name     : Ada.Strings.Unbounded.Unbounded_String;
         Gen      : WL.Random.Names.Name_Generator;
         Paths    : Path_Lists.List;
         Settings : Tropos.Configuration;
      end record;

   procedure Initialize
     (Scenario : in out Hera_Scenario);

end Hera.Scenarios;
