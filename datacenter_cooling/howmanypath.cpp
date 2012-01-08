#include<stdlib.h>
#include<stdio.h>
#include <time.h>

struct pair {unsigned int x; unsigned int y;} start,end;

struct list_el {
  struct pair val;
  struct list_el *next;
};

typedef struct list_el item;

int grid[3][4] = {{ 2, 0, 0, 0 },{ 0, 0, 0, 0 },{ 0, 0, 3, 1 } };

int grid1 [8][7] =
	{
		{2, 0, 0, 0, 0, 0, 0},
		{0, 0, 0, 0, 0, 0, 0},
		{0, 0, 0, 0, 0, 0, 0},
		{0, 0, 0, 0, 0, 0, 0},
		{0, 0, 0, 0, 0, 0, 0},
		{0, 0, 0, 0, 0, 0, 0},
		{0, 0, 0, 0, 0, 0, 0},
		{3, 0, 0, 0, 0, 1, 1}
	};

int n = 3;
int m = 4;
item* nn[3][4];

//int n = 8;
//int m = 7;
//item* nn[8][7];


int len(unsigned int *array){
      return sizeof(array)/sizeof(int);
};

int cc = 0 ;

unsigned int s(unsigned int i,unsigned int j,unsigned int x,unsigned int y){
      return i+x>(-1) && i+x<n && j+y>(-1)  && j+y<m;
}

unsigned int dist(struct pair *a,struct pair *b){
      return abs(a->x-b->x) + abs(a->y-b->y);
}

unsigned int recurse(unsigned int l,unsigned int c,struct pair *b,struct pair *a){

      if(l < ((dist (b, a)) - 1)){ return c; }

      if (l==0){ return c+1; }

      register unsigned int c1 = c;

      item* curr = nn[a->x][a->y];

      grid[a->x][a->y] = 5;

      while(curr!=NULL){
		  if (grid[curr->val.x][curr->val.y]==0) {
			  c1 = c1 + recurse(l-1,c,b,&(curr->val));
		  }
		  curr = curr->next;
      }

      grid[a->x][a->y] = 0;


      return c1;
}

item* kk(item* head,int x, int y){

	item * curr;

	struct pair p = {x,y};
	
	curr = (item *)malloc(sizeof(item));
	curr->val = p;
	curr->next  = head;

	return curr;
}

void memoize(){

      item * curr,* head;
      int i,j;
      
      for(i=0; i<n; i++){
	      for(j=0;j<m;j++){

				  if(grid[i][j]==2){
					  struct pair p = {i,j};
					  start = p;
				  }

				  if(grid[i][j]==3){
					  struct pair p = {i,j};
					  end = p;
				  }

				  if(grid[i][j]==1){
					  cc = cc + 1;
				  }

		      head = NULL;

		      if(s(i,j,0,1)){
			      curr = kk(head,i,j+1);
			      head = curr;
		      }

		      if(s(i,j,0,-1)){
			      curr = kk(head,i,j-1);
			      head = curr;
		      }

		      if(s(i,j,1,0)){
			      curr = kk(head,i+1,j);
			      head = curr;
		      }

		      if(s(i,j,-1,0)){
			      curr = kk(head,i-1,j);
			      head = curr;
		      }


		      nn[i][j] = head;
	      }
      }
}

int main()
{
      memoize();

      time_t t1,t2;
      (void) time(&t1);

      printf("%d solutions.\n", recurse(n*m-(2+cc),0,&end,&start));

      (void) time(&t2);
      printf("time: %d seconds\n", (int) t2-t1);
}
