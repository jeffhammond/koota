#include <stdio.h>
#include <stdlib.h>

#include <mpi.h>

#include "c_prototypes.h"
#include "verify.h"

int main(int argc, char* argv[])
{
    MPI_Init(&argc,&argv);

    const size_t n = (argc > 1) ? atol(argv[1]) : 1000L;

    {
        float * x = calloc(n,sizeof(float));
        float * y = calloc(n,sizeof(float));
        float * yr = calloc(n,sizeof(float));

        for (size_t i=0; i<n; i++) {
            x[i] = i;
            yr[i] = y[i] = 1;
        }

        koota_sum_real4(x,y,n);
        koota_sum_real4(x,yr,n);

        for (size_t i=0; i<n; i++) {
            if (y[i] != yr[i]) {
                printf("%zu %f %f\n",i,y[i],yr[i]);
            }
        }

        free(yr);
        free(y);
        free(x);

    }

    printf("OK\n");
    MPI_Finalize();

    return 0;
}
