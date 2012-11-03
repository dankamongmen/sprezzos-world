PATH		+= /usr/lib/nvidia-cuda-toolkit/bin:

# Work around some strange errors if some headers are found
# in /usr/include only.
INCLUDES	+= -I/usr/lib/nvidia-cuda-toolkit/include

#LIBRARIES	+= $(_SPACE_) -lcudart

#CUDAFE_FLAGS	+=
#OPENCC_FLAGS	+=
#PTXAS_FLAGS	+=
