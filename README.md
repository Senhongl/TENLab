# TENLab

## Introduction
TENLab is an imperative language that supports distributed matrix calculation and its basic syntax is a combination of Python, Matlab and C. In TENLab, everything needs to be wrapped into a tensor, and that's the underlying philosophy of our language, which is to say. In addition to primitive data types, TENLab also provides an advanced data type, i.e., void tensor, which supports user defines and implements some complex data structure.  TENLab also provides an abstract wrapper for users to define their own distributed model for matrix computation. Ideally, TENLab also provides an underlying distributed model for matrix addition, subtraction and multiplication. Overall, the goal is to create a language that is as convenient as Python but supports a parallel matrix computation for acceleration.

## Instruction
* Necessary package installation
```
wget https://download.pytorch.org/libtorch/nightly/cpu/libtorch-shared-with-deps-latest.zip
unzip libtorch-shared-with-deps-latest.zip
rm -rf libtorch-shared-with-deps-latest.zip
```

* To run the code with providing Dockerfile, one can first build the image by (support only for x86-architecture)

```
docker build -t tenlab - < Dockerfile
```

* Then start a container by

```
docker run --rm -it -v $("pwd"):/home/tenlab -w=/home/tenlab tenlab
```

* Inside the docker, compile TENLab and run the tests by

```
make
```

## Team Member
- Xiangrong Xu, xx2367@columbia.edu
- Xincheng Xie, xx2365@columbia.edu 
- Songqing Ye, sy3006@columbia.edu
- Senhong Liu, sl4839@columbia.edu
