with Hera.Calendar;
with Hera.Identifiers;
with Hera.Money;
with Hera.Random;

with Hera.Db.Component;
with Hera.Db.Configured_Component;
with Hera.Db.Configured_Design;
with Hera.Db.Ship;
with Hera.Db.Ship_Component;
with Hera.Db.Ship_Design;

package body Hera.Ships.Configure is

   procedure Create_Configuration
     (Design : Hera.Db.Ship_Design_Reference;
      Config : Tropos.Configuration);

   ----------------------
   -- Configure_Design --
   ----------------------

   procedure Configure_Design (Design_Config : Tropos.Configuration) is
      Design : constant Hera.Db.Ship_Design_Reference :=
        Hera.Db.Ship_Design.Create
          (Name           => Design_Config.Config_Name,
           Dry_Mass       => Design_Config.Get ("mass"),
           Capacity       => Design_Config.Get ("capacity"),
           Crew           => Design_Config.Get ("crew"),
           Weapon_Mounts  => Design_Config.Get ("weapon-mounts", 0),
           Missile_Pylons => Design_Config.Get ("missile-pylons", 0),
           Cost           => Hera.Money.To_Price (Design_Config.Get ("cost")));
   begin
      for Configuration_Config of Design_Config.Child ("configurations") loop
         Create_Configuration (Design, Configuration_Config);
      end loop;
   end Configure_Design;

   -------------------------
   -- Configure_Equipment --
   -------------------------

   procedure Configure_Equipment (Equipment_Config : Tropos.Configuration) is
   begin
      Hera.Db.Component.Create
        (Tag    => Equipment_Config.Config_Name,
         Mass   => Equipment_Config.Get ("mass"),
         Class  =>
           Hera.Db.Component_Class'Value (Equipment_Config.Get ("class")),
         Rating => Equipment_Config.Get ("rating"),
         Price  => Hera.Money.To_Price (Equipment_Config.Get ("price")));
   end Configure_Equipment;

   --------------------------
   -- Create_Configuration --
   --------------------------

   procedure Create_Configuration
     (Design : Hera.Db.Ship_Design_Reference;
      Config : Tropos.Configuration)
   is
      Configuration : constant Hera.Db.Configured_Design_Reference :=
        Hera.Db.Configured_Design.Create
          (Name        => Config.Config_Name,
           Ship_Design => Design);
   begin
      for Component_Config of Config loop
         Hera.Db.Configured_Component.Create
           (Configured_Design => Configuration,
            Component         =>
              Hera.Db.Component.Get_Reference_By_Tag
                (Component_Config.Config_Name));
      end loop;
   end Create_Configuration;

   -----------------
   -- Create_Ship --
   -----------------

   procedure Create_Ship
     (System        : Hera.Db.Star_System_Reference;
      Owner         : Hera.Db.Agent_Reference;
      Ship_Name     : String;
      Design_Name   : String;
      Configuration : String)
   is
      Design : constant Hera.Db.Ship_Design.Ship_Design_Type :=
        Hera.Db.Ship_Design.First_By_Name (Design_Name);
      Config : constant Hera.Db.Configured_Design.Configured_Design_Type :=
        Hera.Db.Configured_Design.Get_By_Configured_Design
          (Design.Get_Ship_Design_Reference, Configuration);
   begin
      if not Design.Has_Element then
         raise Constraint_Error with
           "no such design: " & Design_Name;
      end if;
      if not Config.Has_Element then
         raise Constraint_Error with
           "design '" & Design_Name
           & "' does not have a configuration called '"
           & Configuration & "'";
      end if;

      declare
         use type Hera.Calendar.Time;

         Mass : Non_Negative_Real := Design.Dry_Mass;

         Ship : constant Hera.Db.Ship_Reference :=
           Hera.Db.Ship.Create
             (Name            => Ship_Name,
              Star_System     => System,
              Primary_Massive => Hera.Db.Null_Massive_Object_Reference,
              Semimajor_Axis  => 0.0,
              Epoch           => Hera.Calendar.Clock,
              Eccentricity    => 0.0,
              Period          => 1.0,
              Active          => True,
              Scheduled       => False,
              Next_Event      =>
                Hera.Calendar.Clock
              + Hera.Calendar.Days (Hera.Random.Unit_Random),
              Manager         => "default-trade",
              Updates_Started => False,
              Next_Update     => Hera.Calendar.Clock,
              Mass            => Mass,
              Identifier      => Hera.Identifiers.Next_Identifier,
              Owner           => Owner,
              Ship_Design     => Design.Get_Ship_Design_Reference,
              Status          => Hera.Db.Idle,
              Destination     => Hera.Db.Null_Star_System_Reference,
              Next_Status     => Hera.Calendar.Clock);
      begin
         for Configured_Component of
           Hera.Db.Configured_Component.Select_By_Configured_Design
             (Config.Get_Configured_Design_Reference)
         loop
            Hera.Db.Ship_Component.Create
              (Ship      => Ship,
               Component => Configured_Component.Component,
               Status    => 1.0);
            Mass := Mass +
              Hera.Db.Component.Get (Configured_Component.Component).Mass;
         end loop;

         Hera.Db.Ship.Update_Ship (Ship)
           .Set_Mass (Mass)
           .Done;

      end;

   end Create_Ship;

end Hera.Ships.Configure;
