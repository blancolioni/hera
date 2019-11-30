private with Ada.Containers.Doubly_Linked_Lists;

with Ada.Iterator_Interfaces;

with Hera.Db;

with Hera.Handles.Colony;
with Hera.Handles.Employer;
with Hera.Handles.Pop_Class;

package Hera.Handles.Has_Stock.Selections is

   type Cursor is private;

   function Has_Element (Item : Cursor) return Boolean;

   type Constant_Reference_Type
     (Element : not null access constant Has_Stock_Handle) is private
     with Implicit_Dereference => Element;

   package Selection_Iterator_Interfaces is
     new Ada.Iterator_Interfaces
       (Cursor,
        Has_Element);

   type Selection is tagged limited private
     with Constant_Indexing => Constant_Reference,
     Default_Iterator => Iterate,
     Iterator_Element => Has_Stock_Handle;

   function Iterate
     (Container : Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class;

   function Constant_Reference
     (Container : aliased Selection;
      Position  : Cursor)
      return Constant_Reference_Type;

   function Is_Empty (Container : Selection) return Boolean;

   function Element (Item : Cursor) return Has_Stock_Handle;

   function Select_All return Selection;

   type Has_Stock_Key is tagged private;

   function Top_Record return Has_Stock_Key;
   function Top_Record (Value : Hera.Db.Record_Type) return Has_Stock_Key;

   function Colony return Has_Stock_Key;
   function Colony
     (Value : Hera.Handles.Colony.Colony_Class)
      return Has_Stock_Key;

   function Employment
     (Colony    : Hera.Handles.Colony.Colony_Class;
      Pop_Class : Hera.Handles.Pop_Class.Pop_Class_Class;
      Employer  : Hera.Handles.Employer.Employer_Class)
      return Has_Stock_Key;

   function From return Has_Stock_Key;

   function Select_By (Key : Has_Stock_Key'Class) return Selection;
   function Get_By (Key : Has_Stock_Key'Class) return Has_Stock_Handle;

private

   package Element_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Has_Stock_Handle);

   type Cursor is
      record
         Position : Element_Lists.Cursor;
      end record;

   type Selection is tagged limited
      record
         Data_Source : Element_Lists.List;
      end record;

   type Constant_Reference_Type
     (Element : not null access constant Has_Stock_Handle) is
      record
         Container : access Selection;
      end record;

   type Has_Stock_Key is tagged null record;

end Hera.Handles.Has_Stock.Selections;
