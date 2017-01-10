int m(int x, int y)
{
	return x + y;
}

int double(int temp)
{
	return temp * 2;
}

int fib(int a)
{
	if(a <= 1) { return 1; }
	else { return fib(a-2) + fib(a - 1);}
}

int fact(int a)
{
	if(a <= 1) {return 1;}
	else { return a * fact(a-1);}
}

void main()
{
	int a;
	int b;
	b = 1;
	a = 5 + 3 * (2 + m(double(b), 5) );
	iprint(a);
	return ;
}