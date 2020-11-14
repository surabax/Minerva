#include <stdio.h>
#include <stdint.h>

#define heap_size 32*1024*1024
#define globals_size heap_size

#define word_size 4

#define int int32_t

#define fixnum_mask 3
#define fixnum_tag 0
#define fixnum_shift 2

#define char_mask 255
#define char_tag 15
#define char_shift 8

#define bool_mask 127
#define bool_tag 31
#define bool_shift 7

#define empty_list 47

#define pair_mask 7
#define pair_tag 1
#define pair_shift 3

#define vector_mask 7
#define vector_tag 2
#define vector_shift 3

#define string_mask 7
#define string_tag 3
#define string_shift 3

#define closure_mask 7
#define closure_tag 6
#define closure_shift 3

typedef struct
{
  int * heap;
  int * globals;
} memlayout;

void print(int val)
{
  if((val & fixnum_mask) == fixnum_tag)
    {
      printf("%d", val >> fixnum_shift);
    }
  else if((val & char_mask) == char_tag)
    {
      printf("%c", (char)(val >> char_shift));
    }
  else if((val & bool_mask) == bool_tag)
    {
      if(val >> bool_shift)
	{
	  printf("#t");
	}
      else
	{
	  printf("#f");
	}
    }
  else if(val == empty_list)
    {
      printf("()");
    }
  else if((val & pair_mask) == pair_tag)
    {
      int car = *(int *)(val - pair_tag);
      int cdr = *(int *)(val + word_size - pair_tag);
      printf("(");
      print(car);
      printf(" . ");
      print(cdr);
      printf(")");
    }
  else if((val & vector_mask) == vector_tag)
    {
      int length = *(int *)(val - vector_tag);
      printf("#(");
      int i;
      for(i = word_size; i <= length; i = i + word_size)
	{
	  if(i != word_size) printf(" ");
	  print(*(int *)(val + i - vector_tag));
	}
      printf(")");
    }
  else if((val & string_mask) == string_tag)
    {
      int length = *(int *)(val - string_tag);
      printf("\"");
      int i;
      for(i = word_size; i <= length; i = i + word_size)
	{
	  printf("%c", *(int *)(val + i - string_tag));
	}
      printf("\"");
    }
  else if((val & closure_mask) == closure_tag)
    {
      printf("#<procedure>");
    }
  else
    {
      printf("error: unknown type");
    }
}

int main(int argc, char** argv)
{
  memlayout m;
  
  m.heap = malloc(heap_size);
  m.globals = malloc(globals_size);
  
  int val = scheme_entry(&m);
  
  print(val);
  printf("\n");
  
  free(m.heap);
  free(m.globals);
  
  return 0;
}
