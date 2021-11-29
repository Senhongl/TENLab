obj = *.o

all : $(obj)

%.o : %.c
	cc -c $^

.PHONY : clean
clean :
	rm -rf *.o