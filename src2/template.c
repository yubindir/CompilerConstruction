#include<stdio.h>
#include<stdlib.h>
#include<stdbool.h>

void print(int x){
	printf("%d\n", x);
	exit(0);
}

int main(){
	int y = 260;
	bool x = true;

	true && false;
	false;

	if(x){
		print(y);
	}

	print(y);
	return 1;
}
