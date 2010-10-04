;; RUN: llc -march=x86-64 %s -o /dev/null -print-after=codegenprepare |& FileCheck %s

%struct.Foo = type { i8* }

define %struct.Foo* @_ZN3Foo7collectEj(%struct.Foo* %this, i32 %acc) nounwind
readonly align 2 {
entry:
 br label %tailrecurse

tailrecurse:                                      ; preds = %sw.bb, %entry
 %indvar = phi i64 [ %indvar.next, %sw.bb ], [ 0, %entry ]
 %acc.tr = phi i32 [ %or, %sw.bb ], [ %acc, %entry ]
 %tmp = getelementptr inbounds %struct.Foo* %this, i64 %indvar, i32 0
 %tmp2 = load i8** %tmp, align 8
 %0 = ptrtoint i8* %tmp2 to i64
 %and = and i64 %0, 3
 %conv = trunc i64 %and to i32
 switch i32 %conv, label %sw.epilog [
;; CHECK: %tst = icmp eq i32 %conv, 0
;; CHECK-NEXT: br i1 %tst, label %sw.bb, label %tailrecurse.switch
   i32 0, label %sw.bb
;; CHECK: tailrecurse.switch:
;; CHECK-NEXT: switch i32 %conv, label %sw.epilog
;; CHECK-NEXT: i32 1, label %sw.bb
   i32 1, label %sw.bb
   i32 3, label %sw.bb6
   i32 2, label %sw.bb8
 ]

sw.bb:                                            ; preds = %tailrecurse
 %shl = shl i32 %acc.tr, 1
 %or = or i32 %conv, %shl
 %indvar.next = add i64 %indvar, 1
 br label %tailrecurse

sw.bb6:                                           ; preds = %tailrecurse
 %this.tr.sum21 = add i64 %indvar, 1
 %add.ptr7 = getelementptr inbounds %struct.Foo* %this, i64 %this.tr.sum21
 ret %struct.Foo* %add.ptr7

sw.bb8:                                           ; preds = %tailrecurse
 %idx.ext = zext i32 %acc.tr to i64
 %add.ptr9.sum = add i64 %idx.ext, 1
 %this.tr.sum = add i64 %indvar, %add.ptr9.sum
 %add.ptr11 = getelementptr inbounds %struct.Foo* %this, i64 %this.tr.sum
 ret %struct.Foo* %add.ptr11

sw.epilog:                                        ; preds = %tailrecurse
 ret %struct.Foo* undef
}
