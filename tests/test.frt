int m(int x, int y)
{
	return x + y;
}

int double(int temp)
{
	return temp * 2;
}

int afunc(double fun)
{
	return fun -> int * 2;
}

void main()
{
	int a;
	int b;
	b = 1;
	a = 5 + 3 * (2 + m(double(b), 5) );
	return 0;
}