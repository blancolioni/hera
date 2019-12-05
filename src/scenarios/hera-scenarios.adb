with Ada.Directories;
with Ada.Text_IO;

with Tropos.Reader;

with Hera.Money;
with Hera.Names;

with Hera.Objects;

with Hera.Climate.Configure;
with Hera.Colonies.Configure;
with Hera.Corporations.Configure;
with Hera.Commodities.Configure;
with Hera.Facilities.Configure;
with Hera.Galaxy.Configure;
with Hera.Pops.Classes.Configure;
with Hera.Terrain.Configure;

with Hera.Planets;

with Hera.Corporations.Updates;

with Hera.Scenarios.Converter;
with Hera.Scenarios.Setup;

with Hera.Options;
with Hera.String_Vectors;
with Hera.Paths;

package body Hera.Scenarios is

   Local_Scenario : aliased Hera_Scenario;

   procedure Initialize_Planet
     (Planet            : Hera.Planets.Planet_Type;
      Colony_Config     : Tropos.Configuration;
      Corporation_Count : Positive;
      Start_Cash        : Hera.Money.Money_Type);

   -----------------------
   -- Core_System_Count --
   -----------------------

   function Core_System_Count
     (Scenario : Hera_Scenario)
      return Natural
   is
   begin
      return Scenario.Settings.Get ("core-system-count", 10);
   end Core_System_Count;

   ----------------------
   -- Current_Scenario --
   ----------------------

   function Current_Scenario
     return access constant Hera_Scenario'Class
   is
   begin
      return Local_Scenario'Access;
   end Current_Scenario;

   --------------------
   -- Directory_Path --
   --------------------

   function Directory_Path
     (Scenario       : Hera_Scenario;
      Directory_Name : String)
      return String
   is
   begin
      for Scenario_Path of Scenario.Paths loop
         declare
            use all type Ada.Directories.File_Kind;
            Path : constant String :=
              Ada.Directories.Compose (Scenario_Path, Directory_Name);
         begin
            if Ada.Directories.Exists (Path)
              and then Ada.Directories.Kind (Path) = Directory
            then
               return Path;
            end if;
         end;
      end loop;
      return "";
   end Directory_Path;

   ---------------
   -- File_Path --
   ---------------

   function File_Path
     (Scenario  : Hera_Scenario;
      File_Name : String)
      return String
   is
   begin
      for Scenario_Path of Scenario.Paths loop
         declare
            use all type Ada.Directories.File_Kind;
            Path : constant String :=
              Ada.Directories.Compose (Scenario_Path, File_Name);
         begin
            if Ada.Directories.Exists (Path)
              and then Ada.Directories.Kind (Path) = Ordinary_File
            then
               return Path;
            end if;
         end;
      end loop;
      return "";
   end File_Path;

   ---------------
   -- File_Path --
   ---------------

   function File_Path
     (Scenario       : Hera_Scenario;
      Directory_Name : String;
      File_Name      : String)
      return String
   is
   begin
      for Scenario_Path of Scenario.Paths loop
         declare
            use all type Ada.Directories.File_Kind;
            Path : constant String :=
              Ada.Directories.Compose
                (Ada.Directories.Compose
                   (Scenario_Path, Directory_Name),
                 File_Name);
         begin
            if Ada.Directories.Exists (Path)
              and then Ada.Directories.Kind (Path) = Ordinary_File
            then
               return Path;
            end if;
         end;
      end loop;
      return "";
   end File_Path;

   -----------------
   -- Galaxy_Axes --
   -----------------

   procedure Galaxy_Axes
     (Scenario : Hera_Scenario;
      X, Y, Z  : out Non_Negative_Real)
   is
      Size_Config : constant Tropos.Configuration :=
        Scenario.Settings.Child ("galaxy-size");
   begin
      X := Size_Config.Get (1);
      Y := Size_Config.Get (2);
      Z := Size_Config.Get (3);
   end Galaxy_Axes;

   --------------------------
   -- Initial_System_Count --
   --------------------------

   function Initial_System_Count
     (Scenario : Hera_Scenario)
      return Natural
   is
   begin
      return Scenario.Settings.Get
        ("initial-system-count",
         Scenario.Core_System_Count * 10);
   end Initial_System_Count;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Scenario : in out Hera_Scenario)
   is
      Axis_X_Radius : Non_Negative_Real;
      Axis_Y_Radius : Non_Negative_Real;
      Axis_Z_Radius : Non_Negative_Real;

   begin

      Ada.Text_IO.Put_Line
        ("initializing scenario: "
         & Ada.Strings.Unbounded.To_String (Scenario.Name));

      WL.Random.Names.Load_Lexicon
        (Scenario.Gen,
         Scenario.File_Path ("system-name-vowels.txt"),
         Scenario.File_Path ("system-name-consonants.txt"));

      Scenario.Galaxy_Axes (Axis_X_Radius, Axis_Y_Radius, Axis_Z_Radius);

      if Hera.Options.Import_Properties then
         Ada.Text_IO.Put_Line ("importing properties");
         declare
            Item_Properties_Path : constant String :=
              Scenario.File_Path ("starcorp", "items.properties");
            Facility_Properties_Path : constant String :=
              Scenario.File_Path ("starcorp", "facilities.properties");
            Pop_Class_Properties_Path  : constant String :=
              Scenario.File_Path ("starcorp", "population.properties");
            Terrain_Properties_Path  : constant String :=
              Scenario.File_Path ("starcorp", "terrain.properties");
         begin
            if Item_Properties_Path /= "" then
               Hera.Scenarios.Converter.Convert_Items_To_Commodities
                 (Item_Properties_Path,
                  Scenario.Directory_Path ("commodities"));
            end if;
            if Facility_Properties_Path /= "" then
               Hera.Scenarios.Converter.Convert_Facilities
                 (Facility_Properties_Path,
                  Scenario.Directory_Path ("facilities"));
            end if;
            if Pop_Class_Properties_Path /= "" then
               Hera.Scenarios.Converter.Convert_Pop_Classes
                 (Pop_Class_Properties_Path,
                  Scenario.Directory_Path ("pops"));
            end if;
            if Terrain_Properties_Path /= "" then
               Hera.Scenarios.Converter.Convert_Terrain
                 (Terrain_Properties_Path,
                  Scenario.Directory_Path ("terrain"));
            end if;
         end;
      end if;

      Ada.Text_IO.Put_Line ("initializing base tables");

      Hera.Names.Configure_Names
        (Tropos.Reader.Read_Config
           (Scenario.File_Path ("names.txt")));

      Scenario.Load ("commodities", "commodity",
                     Hera.Commodities.Configure.Create_Commodity'Access);

      Scenario.Load ("commodities", "commodity",
                     Hera.Commodities.Configure.Create_Components'Access);
      Hera.Commodities.Configure.Create_Prices;

      Scenario.Load ("facilities", "facility",
                     Hera.Facilities.Configure.Create_Services'Access);

      Scenario.Load ("pops", "pop",
                     Hera.Pops.Classes.Configure.Create_Pop_Class'Access);

      Scenario.Load ("facilities", "facility",
                     Hera.Facilities.Configure.Create_Facility'Access);

      Scenario.Load ("terrain", "terrain",
                     Hera.Terrain.Configure.Create_Terrain'Access);

      Scenario.Load ("climate", "climate",
                     Hera.Climate.Configure.Create_Climate'Access);

      Scenario.Load ("colonies", "colony",
                     Hera.Colonies.Configure.Configure_Template'Access);

      Hera.Galaxy.Configure.Generate_Galaxy
        (Number_Of_Systems => Scenario.Initial_System_Count,
         Radius_X          => Axis_X_Radius,
         Radius_Y          => Axis_Y_Radius,
         Radius_Z          => Axis_Z_Radius,
         Names             => Scenario.Gen);

      Ada.Text_IO.Put_Line
        ("Creating initial corporations");

      declare

         Major_Count : constant Natural :=
           Scenario.Settings.Child
             ("major-corporations")
           .Get ("count", 1);

         Major_Cash : constant Hera.Money.Money_Type :=
           Hera.Money.To_Money
             (Scenario.Settings.Child ("major-corporations")
              .Get ("start-cash"));

         Minor_Count : constant Natural :=
           Scenario.Settings.Child
             ("minor-corporations")
           .Get ("count", 1);

         Minor_Cash : constant Hera.Money.Money_Type :=
           Hera.Money.To_Money
             (Scenario.Settings.Child ("minor-corporations")
              .Get ("start-cash"));

         function Habitable
           (Planet : Hera.Planets.Planet_Type)
            return Non_Negative_Real
         is (if Planet.Habitability >= 0.75
             then Planet.Habitability
             else 0.0);

         Start_Planets : constant Hera.Planets.Planet_Array :=
                           Hera.Planets.Find (Habitable'Access);

         Planet_Index  : Positive := Start_Planets'First;

      begin

         Initialize_Planet
           (Start_Planets (Planet_Index),
            Scenario.Settings.Child ("populated-planet-colonies"),
            Major_Count, Major_Cash);

         Planet_Index := Planet_Index + 1;

         for I in 1 .. Minor_Count loop
            Initialize_Planet
              (Planet            => Start_Planets (Planet_Index),
               Colony_Config     =>
                 Scenario.Settings.Child ("initial-planet-colonies"),
               Corporation_Count => 1,
               Start_Cash        => Minor_Cash);
            Planet_Index := Planet_Index + 1;
         end loop;

      end;

      Local_Scenario := Scenario;

   end Initialize;

   -----------------------
   -- Initialize_Planet --
   -----------------------

   procedure Initialize_Planet
     (Planet            : Hera.Planets.Planet_Type;
      Colony_Config     : Tropos.Configuration;
      Corporation_Count : Positive;
      Start_Cash        : Hera.Money.Money_Type)
   is
      Corps         : Hera.Corporations.Corporation_Array
        (1 .. Corporation_Count);
      Colonies      : Hera.String_Vectors.Vector;

   begin

      Planet.Set_Colonized;

      for Template_Config of Colony_Config loop
         for I in 1 .. Template_Config.Value loop
            Colonies.Append (Template_Config.Config_Name);
         end loop;
      end loop;

      for I in 1 .. Corporation_Count loop
         Corps (I) :=
           Hera.Corporations.Configure.Create
             (Planet     => Planet,
              Start_Cash => Start_Cash);
      end loop;

      Hera.Scenarios.Setup.Initial_Planet_Setup
        (Planet           => Planet,
         Corporations     => Corps,
         Colony_Templates => Colonies);

      Hera.Objects.Apply_Updates;

      declare
         procedure Initial_Update
           (Colony : Hera.Colonies.Colony_Type);

         --------------------
         -- Initial_Update --
         --------------------

         procedure Initial_Update
           (Colony : Hera.Colonies.Colony_Type)
         is
         begin
            for Corp of Corps loop
               Hera.Corporations.Updates.Sell_Commodities
                 (Corp, Colony);
            end loop;
         end Initial_Update;

      begin
         Hera.Colonies.Iterate (Initial_Update'Access);
      end;

      Hera.Objects.Apply_Updates;
   end Initialize_Planet;

   ----------
   -- Load --
   ----------

   procedure Load
     (Scenario       : Hera_Scenario;
      Directory_Name : String;
      Extension      : String;
      Loader         : not null access
        procedure (Config : Tropos.Configuration))
   is
      Path : constant String := Scenario.Directory_Path (Directory_Name);
   begin
      if Path = "" then
         raise Constraint_Error with
           "unable to find directory " & Directory_Name
           & " for scenario "
           & Ada.Strings.Unbounded.To_String (Scenario.Name);
      end if;

      Ada.Text_IO.Put_Line
        ("loading " & Directory_Name & " ...");

      Tropos.Reader.Read_Config
        (Path      => Path,
         Extension => Extension,
         Configure => Loader);

   end Load;

   ---------------------------
   -- Non_Core_System_Count --
   ---------------------------

   function Non_Core_System_Count
     (Scenario : Hera_Scenario)
      return Natural
   is
   begin
      return Scenario.Settings.Get ("non-core-system-count", 50);
   end Non_Core_System_Count;

   -------------------
   -- Open_Scenario --
   -------------------

   function Open_Scenario (Name : String) return Hera_Scenario is
   begin
      if not Ada.Directories.Exists
        (Hera.Paths.Config_File ("scenarios/" & Name))
      then
         raise Constraint_Error with Name & ": scenario not found";
      end if;

      if not Ada.Directories.Exists
        (Hera.Paths.Config_File
           ("scenarios/" & Name & "/" & Name & ".scenario"))
      then
         raise Constraint_Error with
         Name & ": missing " & Name & ".scenario";
      end if;

      return Scenario : Hera_Scenario do
         Scenario.Settings :=
           Tropos.Reader.Read_Config
             (Hera.Paths.Config_File
                ("scenarios/" & Name & "/" & Name & ".scenario"));
         Scenario.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
         Scenario.Paths.Append
           (Hera.Paths.Config_File ("scenarios/" & Name));
         Scenario.Paths.Append
           (Hera.Paths.Config_Path);

         Scenario.Initialize;

      end return;

   end Open_Scenario;

end Hera.Scenarios;
