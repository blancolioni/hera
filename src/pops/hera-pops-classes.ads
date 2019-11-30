private with Ada.Containers.Doubly_Linked_Lists;

with Hera.Objects;

with Hera.Accounts;

with Hera.Money;

with Hera.Commodities;

package Hera.Pops.Classes is

   type Root_Pop_Class_Object is
     new Hera.Objects.Root_Localised_Object
   with private;

   function Consumer_Quality
     (Pop_Class : Root_Pop_Class_Object'Class)
      return Quality_Type;

   function Service_Quality
     (Pop_Class : Root_Pop_Class_Object'Class)
      return Quality_Type;

   function Salary (Pop_Class : Root_Pop_Class_Object'Class)
                    return Hera.Money.Price_Type;

   procedure Iterate_Required_Goods
     (Pop_Class : Root_Pop_Class_Object'Class;
      Process   : not null access
        procedure (Commodity : Hera.Commodities.Commodity_Type));

   type Pop_Class_Type is access constant Root_Pop_Class_Object'Class;

   function Exists (Tag : String) return Boolean;
   function Get (Tag : String) return Pop_Class_Type
     with Pre => Exists (Tag);

   type Pop_Class_Array is array (Positive range <>) of Pop_Class_Type;

private

   package Commodity_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Hera.Commodities.Commodity_Type, Hera.Commodities."=");

   type Root_Pop_Class_Object is
     new Hera.Objects.Root_Localised_Object with
      record
         Consumer_Quality : Quality_Type;
         Service_Quality  : Quality_Type;
         Salary           : Hera.Money.Price_Type;
         Required_Goods   : Commodity_Lists.List;
      end record;

   function Consumer_Quality
     (Pop_Class : Root_Pop_Class_Object'Class)
      return Quality_Type
   is (Pop_Class.Consumer_Quality);

   function Service_Quality
     (Pop_Class : Root_Pop_Class_Object'Class)
      return Quality_Type
   is (Pop_Class.Service_Quality);

   function Salary (Pop_Class : Root_Pop_Class_Object'Class)
                    return Hera.Money.Price_Type
   is (Pop_Class.Salary);

   procedure New_Pop_Class (Pop_Class : Pop_Class_Type);

end Hera.Pops.Classes;
