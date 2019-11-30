private with Ada.Containers.Doubly_Linked_Lists;

with Ada.Iterator_Interfaces;

with Hera.Db;

package Hera.Handles.Account_History.Selections is

   type Cursor is private;

   function Has_Element (Item : Cursor) return Boolean;

   type Constant_Reference_Type
     (Element : not null access constant Account_History_Handle) is private
     with Implicit_Dereference => Element;

   package Selection_Iterator_Interfaces is
     new Ada.Iterator_Interfaces
       (Cursor,
        Has_Element);

   type Selection is tagged limited private
     with Constant_Indexing => Constant_Reference,
     Default_Iterator => Iterate,
     Iterator_Element => Account_History_Handle;

   function Iterate
     (Container : Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class;

   function Constant_Reference
     (Container : aliased Selection;
      Position  : Cursor)
      return Constant_Reference_Type;

   function Is_Empty (Container : Selection) return Boolean;

   function Element (Item : Cursor) return Account_History_Handle;

   function Select_All return Selection;

   type Account_History_Key (<>) is tagged private;

   function Top_Record return Account_History_Key;
   function Top_Record
     (Value : Hera.Db.Record_Type)
      return Account_History_Key;

   function Identifier return Account_History_Key;
   function Identifier (Value : String) return Account_History_Key;

   function Account_History
     (Account    : Hera.Handles.Account.Account_Class;
      Time_Stamp : Hera.Calendar.Time)
      return Account_History_Key;

   function In_Range
     (From, To : Account_History_Key)
      return Account_History_Key;

   function From (Key : Account_History_Key) return Account_History_Key;
   function To (Key : Account_History_Key) return Account_History_Key;

   function Select_By (Key : Account_History_Key'Class) return Selection;
   function Select_Range
     (From, To : Account_History_Key'Class) return Selection;

   function Get_By
     (Key : Account_History_Key'Class)
      return Account_History_Handle;

private

   package Element_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Account_History_Handle);

   type Cursor is
      record
         Position : Element_Lists.Cursor;
      end record;

   type Selection is tagged limited
      record
         Data_Source : Element_Lists.List;
      end record;

   type Constant_Reference_Type
     (Element : not null access constant Account_History_Handle) is
      record
         Container : access Selection;
      end record;

   type Account_History_Key is tagged null record;

end Hera.Handles.Account_History.Selections;
