#include <stdio.h>
__global__ void kernel() {}
int main() {
  kernel<<<1,1>>>();
  cudaDeviceSynchronize();
  printf("CUDA OK\n");
  return 0;
}
