private with Ada.Containers.Doubly_Linked_Lists;

with Tropos;

with Hera.Calendar;

with Hera.Objects;

package Hera.Star_Systems is

   type Root_Star_System_Entity is
     abstract new Hera.Objects.Root_Named_Object with private;

   function Mass
     (Entity : Root_Star_System_Entity'Class)
      return Non_Negative_Real;

   function Radius
     (Entity : Root_Star_System_Entity'Class)
      return Non_Negative_Real;

   function Colonized
     (Entity : Root_Star_System_Entity'Class)
      return Boolean;

   procedure Set_Colonized
     (Entity : Root_Star_System_Entity'Class);

   procedure On_Colonized
     (Entity : in out Root_Star_System_Entity)
   is null;

   procedure Add_Orbiting_Entity
     (To     : Root_Star_System_Entity'Class;
      Entity : not null access constant Root_Star_System_Entity'Class);

   overriding procedure Save
     (Object : Root_Star_System_Entity;
      Config : in out Tropos.Configuration);

   type Star_System_Entity is access constant Root_Star_System_Entity'Class;

   function Primary
     (Entity : Root_Star_System_Entity'Class)
      return Star_System_Entity;

   type Root_Star_System_Type is
     new Hera.Objects.Root_Named_Object with private;

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
      Surface_Gravity : Non_Negative_Real);

   function X (Star_System : Root_Star_System_Type'Class) return Real;
   function Y (Star_System : Root_Star_System_Type'Class) return Real;
   function Z (Star_System : Root_Star_System_Type'Class) return Real;

   function Primary
     (Star_System : Root_Star_System_Type'Class)
      return Star_System_Entity;

   procedure Iterate_Entities
     (Star_System : Root_Star_System_Type'Class;
      Process     : not null access
        procedure (Entity : Star_System_Entity));

   type Star_System_Type is access constant Root_Star_System_Type;

   function System (Entity : Root_Star_System_Entity'Class)
                    return Star_System_Type;

   function Distance (From, To : Star_System_Type) return Non_Negative_Real;

   procedure Iterate
     (Process : not null access
        procedure (Star_System : Star_System_Type));

   procedure Save
     (Config : in out Tropos.Configuration);

private

   package Star_System_Entity_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Star_System_Entity);

   type Root_Star_System_Entity is
     abstract new Hera.Objects.Root_Named_Object with
      record
         System          : Star_System_Type;
         Primary         : Star_System_Entity;
         Mass            : Non_Negative_Real;
         Radius          : Non_Negative_Real;
         Orbit           : Non_Negative_Real;
         Epoch           : Hera.Calendar.Time;
         Density         : Non_Negative_Real;
         Rotation_Period : Non_Negative_Real;
         Tilt            : Real;
         Surface_Gravity : Non_Negative_Real;
         Has_Colony      : Boolean;
         Dependents      : Star_System_Entity_Lists.List;
      end record;

   function Mass
     (Entity : Root_Star_System_Entity'Class)
      return Non_Negative_Real
   is (Entity.Mass);

   function Radius
     (Entity : Root_Star_System_Entity'Class)
      return Non_Negative_Real
   is (Entity.Radius);

   function Primary
     (Entity : Root_Star_System_Entity'Class)
      return Star_System_Entity
   is (Entity.Primary);

   function System
     (Entity : Root_Star_System_Entity'Class)
      return Star_System_Type
   is (Entity.System);

   function Colonized
     (Entity : Root_Star_System_Entity'Class)
      return Boolean
   is (Entity.Has_Colony
       or else (for some Dep of Entity.Dependents => Dep.Colonized));

   type Root_Star_System_Type is
     new Hera.Objects.Root_Named_Object with
      record
         X, Y, Z  : Real;
         Primary  : Star_System_Entity;
      end record;

   overriding procedure Save
     (Object : Root_Star_System_Type;
      Config : in out Tropos.Configuration);

   function X (Star_System : Root_Star_System_Type'Class) return Real
   is (Star_System.X);

   function Y (Star_System : Root_Star_System_Type'Class) return Real
   is (Star_System.Y);

   function Z (Star_System : Root_Star_System_Type'Class) return Real
   is (Star_System.Z);

   function Primary
     (Star_System : Root_Star_System_Type'Class)
      return Star_System_Entity
   is (Star_System.Primary);

   function New_Star_System
     (Name    : String;
      X, Y, Z : Real)
      return Star_System_Type;

   procedure Set_Primary
     (Star_System : Star_System_Type;
      Primary     : not null access constant Root_Star_System_Entity'Class);

end Hera.Star_Systems;
