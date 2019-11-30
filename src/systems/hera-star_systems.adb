with Ada.Containers.Vectors;

with Hera.Elementary_Functions;

package body Hera.Star_Systems is

   Star_System_Version : constant Hera.Objects.Object_Version := "0.0.1";

   type Set_Primary_Update is
     new Hera.Objects.Root_Update_Type with
      record
         Primary : Star_System_Entity;
      end record;

   overriding procedure Execute
     (Update : Set_Primary_Update;
      Object : in out Hera.Objects.Root_Hera_Object'Class);

   overriding function Description
     (Update : Set_Primary_Update)
      return String
   is ("set primary to " & Update.Primary.Name);

   type Add_Orbiting_Entity_Update is
     new Hera.Objects.Root_Update_Type with
      record
         Entity : Star_System_Entity;
      end record;

   overriding procedure Execute
     (Update : Add_Orbiting_Entity_Update;
      Object : in out Hera.Objects.Root_Hera_Object'Class);

   overriding function Description
     (Update : Add_Orbiting_Entity_Update)
      return String
   is ("add " & Update.Entity.Name & " to orbit");

   type Set_Colonized_Update is
     new Hera.Objects.Root_Update_Type with
      record
         null;
      end record;

   overriding procedure Execute
     (Update : Set_Colonized_Update;
      Object : in out Hera.Objects.Root_Hera_Object'Class);

   overriding function Description
     (Update : Set_Colonized_Update)
      return String
   is ("set colonized");

   package Star_System_Vectors is
     new Ada.Containers.Vectors (Positive, Star_System_Type);

   Vector : Star_System_Vectors.Vector;

   -------------------------
   -- Add_Orbiting_Entity --
   -------------------------

   procedure Add_Orbiting_Entity
     (To     : Root_Star_System_Entity'Class;
      Entity : not null access constant Root_Star_System_Entity'Class)
   is
   begin
      To.Add_Update
        (Add_Orbiting_Entity_Update'
           (Hera.Objects.Root_Update_Type with
                Entity => Star_System_Entity (Entity)));
   end Add_Orbiting_Entity;

   --------------
   -- Distance --
   --------------

   function Distance (From, To : Star_System_Type) return Non_Negative_Real is
      use Hera.Elementary_Functions;
   begin
      return Sqrt ((From.X - To.X) ** 2
                   + (From.Y - To.Y) ** 2
                   + (From.Z - To.Z) ** 2);
   end Distance;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Set_Primary_Update;
      Object : in out Hera.Objects.Root_Hera_Object'Class)
   is
   begin
      Root_Star_System_Type (Object).Primary := Update.Primary;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Add_Orbiting_Entity_Update;
      Object : in out Hera.Objects.Root_Hera_Object'Class)
   is
   begin
      Root_Star_System_Entity (Object).Dependents.Append (Update.Entity);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Set_Colonized_Update;
      Object : in out Hera.Objects.Root_Hera_Object'Class)
   is
      pragma Unreferenced (Update);
      Entity : Root_Star_System_Entity'Class renames
        Root_Star_System_Entity'Class (Object);
   begin
      if not Entity.Has_Colony then
         Entity.Has_Colony := True;
         Entity.On_Colonized;
      end if;
   end Execute;

   -----------------------------------
   -- Initialize_Star_System_Entity --
   -----------------------------------

   procedure Initialize_Star_System_Entity
     (Entity          : in out Root_Star_System_Entity'Class;
      System          : not null access constant Root_Star_System_Type'Class;
      Name            : String;
      Version         : Hera.Objects.Object_Version;
      Mass            : Non_Negative_Real;
      Primary         : access constant Root_Star_System_Entity'Class;
      Orbit           : Non_Negative_Real;
      Epoch           : Hera.Calendar.Time;
      Radius          : Non_Negative_Real;
      Density         : Non_Negative_Real;
      Rotation_Period : Non_Negative_Real;
      Tilt            : Real;
      Surface_Gravity : Non_Negative_Real)
   is
   begin
      Entity.Initialize (Version, Name);
      Entity.System := Star_System_Type (System);
      Entity.Mass := Mass;
      Entity.Primary := Star_System_Entity (Primary);
      Entity.Orbit := Orbit;
      Entity.Epoch := Epoch;
      Entity.Radius := Radius;
      Entity.Density := Density;
      Entity.Rotation_Period := Rotation_Period;
      Entity.Tilt := Tilt;
      Entity.Surface_Gravity := Surface_Gravity;
      Entity.Has_Colony := False;
   end Initialize_Star_System_Entity;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Process : not null access
        procedure (Star_System : Star_System_Type))
   is
   begin
      for Item of Vector loop
         Process (Item);
      end loop;
   end Iterate;

   procedure Iterate_Entities
     (Star_System : Root_Star_System_Type'Class;
      Process     : not null access
        procedure (Entity : Star_System_Entity))
   is
      procedure Go (E : not null access constant
                      Root_Star_System_Entity'Class);

      --------
      -- Go --
      --------

      procedure Go (E : not null access constant
                      Root_Star_System_Entity'Class)
      is
      begin
         Process (Star_System_Entity (E));
         for Dep of E.Dependents loop
            Go (Dep);
         end loop;
      end Go;

   begin
      Go (Star_System.Primary);
   end Iterate_Entities;

   ---------------------
   -- New_Star_System --
   ---------------------

   function New_Star_System
     (Name    : String;
      X, Y, Z : Real)
      return Star_System_Type
   is
      Rec : constant Root_Star_System_Type :=
        Root_Star_System_Type'
          (Hera.Objects.Root_Named_Object with
           X       => X,
           Y       => Y,
           Z       => Z,
           Primary => null);

      New_Star_System : constant Star_System_Type :=
        Star_System_Type (Rec.New_Named_Object (Star_System_Version, Name));
   begin
      Vector.Append (New_Star_System);
      return New_Star_System;
   end New_Star_System;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Object : Root_Star_System_Entity;
      Config : in out Tropos.Configuration)
   is
      procedure Save
        (Name : String;
         X    : Real);

      ----------
      -- Save --
      ----------

      procedure Save
        (Name : String;
         X    : Real)
      is
      begin
         Config.Add (Name, X);
      end Save;

   begin
      Hera.Objects.Root_Named_Object (Object).Save (Config);
      Save ("mass", Object.Mass);
      Save ("radius", Object.Radius);
      Save ("density", Object.Density);
      Save ("period", Object.Rotation_Period);
      Save ("tilt", Object.Tilt);
      Save ("surface-g", Object.Surface_Gravity);

      declare
         Deps_Config : Tropos.Configuration :=
           Tropos.New_Config ("dependents");
         Index       : Natural := 0;
      begin
         for Item of Object.Dependents loop
            Index := Index + 1;
            declare
               Dep : Tropos.Configuration := Tropos.New_Config (Index);
            begin
               Item.Save (Dep);
               Deps_Config.Add (Dep);
            end;
         end loop;
         Config.Add (Deps_Config);
      end;

   end Save;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Object : Root_Star_System_Type;
      Config : in out Tropos.Configuration)
   is
      Primary_Config : Tropos.Configuration :=
        Tropos.New_Config ("primary");
   begin
      Hera.Objects.Root_Named_Object (Object).Save (Config);
      Config.Add ("x", Float (Object.X));
      Config.Add ("y", Float (Object.Y));
      Config.Add ("z", Float (Object.Z));
      Object.Primary.Save (Primary_Config);
      Config.Add (Primary_Config);
   end Save;

   ----------
   -- Save --
   ----------

   procedure Save
     (Config : in out Tropos.Configuration)
   is
      Systems_Config : Tropos.Configuration :=
        Tropos.New_Config ("systems");
   begin
      for I in 1 .. Vector.Last_Index loop
         declare
            Sys_Config : Tropos.Configuration :=
              Tropos.New_Config (I);
         begin
            Vector.Element (I).Save (Sys_Config);
            Systems_Config.Add (Sys_Config);
         end;
      end loop;
      Config.Add (Systems_Config);
   end Save;

   -------------------
   -- Set_Colonized --
   -------------------

   procedure Set_Colonized
     (Entity : Root_Star_System_Entity'Class)
   is
   begin
      if not Entity.Colonized then
         Entity.Add_Update
           (Set_Colonized_Update'
              (Hera.Objects.Root_Update_Type with null record));
      end if;
   end Set_Colonized;

   -----------------
   -- Set_Primary --
   -----------------

   procedure Set_Primary
     (Star_System : Star_System_Type;
      Primary     : not null access constant Root_Star_System_Entity'Class)
   is
   begin
      Star_System.Add_Update
        (Set_Primary_Update'
           (Hera.Objects.Root_Update_Type with
                Primary => Star_System_Entity (Primary)));
   end Set_Primary;

end Hera.Star_Systems;
