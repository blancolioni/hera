private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Holders;

with Ada.Iterator_Interfaces;

with Hera.Db;

with Hera.Handles.Has_Account;

package Hera.Handles.Salary_Transaction.MSelections is

   type Cursor is private;

   function Has_Element (Item : Cursor) return Boolean;

   type Constant_Reference_Type
     (Element : not null access constant Salary_Transaction_Handle) is private
     with Implicit_Dereference => Element;

   package Selection_Iterator_Interfaces is
     new Ada.Iterator_Interfaces
       (Cursor,
        Has_Element);

   type Selection is tagged limited private
     with Constant_Indexing => Constant_Reference,
     Default_Iterator => Iterate,
     Iterator_Element => Salary_Transaction_Handle;

   function Iterate
     (Container : Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class;

   function Constant_Reference
     (Container : aliased Selection;
      Position  : Cursor)
      return Constant_Reference_Type;

   function Is_Empty (Container : Selection) return Boolean;

   function Element (Item : Cursor) return Salary_Transaction_Handle;

   function Select_All return Selection;

   type Selection_Condition is tagged private;

   function "and"
     (Left, Right : Selection_Condition)
      return Selection_Condition;

   function Buyer
     (Value : Hera.Handles.Has_Account.Has_Account_Class)
      return Selection_Condition;

   type Time_Stamp_Field is private;

   function Time_Stamp
      return Time_Stamp_Field;

   function "="
     (Left  : Time_Stamp_Field;
      Right : Hera.Calendar.Time)
      return Selection_Condition;

   function "/="
     (Left  : Time_Stamp_Field;
      Right : Hera.Calendar.Time)
      return Selection_Condition;

   function "<"
     (Left : Time_Stamp_Field;
      Right : Hera.Calendar.Time)
      return Selection_Condition;

   function "<="
     (Left  : Time_Stamp_Field;
      Right : Hera.Calendar.Time)
      return Selection_Condition;

   function ">"
     (Left  : Time_Stamp_Field;
      Right : Hera.Calendar.Time)
      return Selection_Condition;

   function ">="
     (Left  : Time_Stamp_Field;
      Right : Hera.Calendar.Time)
      return Selection_Condition;

   function Time_Stamp
     (Value : Hera.Calendar.Time)
      return Selection_Condition;

   function Select_Where
     (Condition : Selection_Condition'Class)
      return Selection;

   function Get_Where
     (Condition : Selection_Condition'Class)
      return Salary_Transaction_Handle;

   function First_Where
     (Condition : Selection_Condition'Class)
      return Salary_Transaction_Handle;

private

   package Element_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Salary_Transaction_Handle);

   type Cursor is
      record
         Position : Element_Lists.Cursor;
      end record;

   type Selection is tagged limited
      record
         Data_Source : Element_Lists.List;
      end record;

   type Constant_Reference_Type
     (Element : not null access constant Salary_Transaction_Handle) is
      record
         null;
      end record;

   type Selection_Key_Type is
     (K_No_Selection_Key, K_Top_Record, K_Buyer, K_Seller);

   type Selection_Key (Key_Type : Selection_Key_Type) is
      record
         Has_Value : Boolean;
         case Key_Type is
            when K_No_Selection_Key =>
               null;
            when K_Top_Record =>
               Top_Record_Value : Hera.Db.Record_Type;
            when K_Buyer =>
               Buyer_Value      : Hera.Db.Has_Account_Reference;
            when K_Seller =>
               Seller_Value     : Hera.Db.Has_Account_Reference;
         end case;
      end record;

   package Selection_Key_Holders is
     new Ada.Containers.Indefinite_Holders (Selection_Key);

   type Constraint_Field is
     (C_Top_Record,
      C_Kit_Root_Record,
      C_Time_Stamp,
      C_Buyer,
      C_Seller,
      C_Amount,
      C_Transaction);

   type Constraint_Operator is
     (Op_None, Op_Not,
      Op_EQ, Op_NE,
      Op_GT, Op_LT, Op_GE, Op_LE);

   type Selection_Constraint (Field : Constraint_Field) is
      record
         Operator : Constraint_Operator;
         case Field is
            when C_Top_Record =>
               Top_Record_Value      : Hera.Db.Record_Type;
            when C_Kit_Root_Record =>
               Kit_Root_Record_Value : Hera.Db.Kit_Root_Record_Reference;
            when C_Time_Stamp =>
               Time_Stamp_Value      : Hera.Calendar.Time;
            when C_Buyer =>
               Buyer_Value           : Hera.Db.Has_Account_Reference;
            when C_Seller =>
               Seller_Value          : Hera.Db.Has_Account_Reference;
            when C_Amount =>
               Amount_Value          : Hera.Money.Money_Type;
            when C_Transaction =>
               Transaction_Value     : Hera.Db.Transaction_Reference;
         end case;
      end record;

   package Selection_Constraint_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Selection_Constraint);

   type Selection_Condition is tagged
      record
         Main_Key    : Selection_Key_Holders.Holder;
         Constraints : Selection_Constraint_Lists.List;
      end record;

   type Time_Stamp_Field is null record;

end Hera.Handles.Salary_Transaction.MSelections;
