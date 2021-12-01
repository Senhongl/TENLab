; ModuleID = 'TensorC'
source_filename = "TensorC"

%tensor_t = type { i8, i8, i8*, i8* }

@sdim = private unnamed_addr constant [2 x i8] c"\02\03"
@sdata = private unnamed_addr constant [6 x i32] [i32 1, i32 2, i32 3, i32 4, i32 1, i32 0]
@stensor = private unnamed_addr constant %tensor_t { i8 0, i8 2, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @sdim, i32 0, i32 0), i8* bitcast ([6 x i32]* @sdata to i8*) }
@sdim.1 = private unnamed_addr constant [2 x i8] c"\02\03"
@sdata.2 = private unnamed_addr constant [6 x i32] [i32 1, i32 0, i32 5, i32 0, i32 2, i32 0]
@stensor.3 = private unnamed_addr constant %tensor_t { i8 0, i8 2, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @sdim.1, i32 0, i32 0), i8* bitcast ([6 x i32]* @sdata.2 to i8*) }

define i8 @main() {
entry:
  %tmpOp = call i8* @subtract(i8* getelementptr inbounds (%tensor_t, %tensor_t* @stensor, i32 0, i32 0), i8* getelementptr inbounds (%tensor_t, %tensor_t* @stensor.3, i32 0, i32 0))
  call void @print(i8* %tmpOp)
  ret i8 0
}

declare i8* @add(i8*, i8*)

declare i8* @subtract(i8*, i8*)

declare i8* @mult(i8*, i8*)

declare i8* @dotmul(i8*, i8*)

declare i8* @divide(i8*, i8*)

declare i8* @floordivide(i8*, i8*)

declare i8* @matpow(i8*, i8*)

declare i8* @dotpow(i8*, i8*)

declare i8* @mod(i8*, i8*)

declare i8* @transpose(i8*)

declare i8* @equal(i8*, i8*)

declare i8* @notequal(i8*, i8*)

declare i8* @greater(i8*, i8*)

declare i8* @greaterequal(i8*, i8*)

declare i8* @less(i8*, i8*)

declare i8* @lessequal(i8*, i8*)

declare void @print(i8*)

declare i8* @logicaland(i8*, i8*)

declare i8* @logicalor(i8*, i8*)
