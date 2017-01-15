int fact(int a)
{
	if(a <= 1) { return 1; }
	else { return a * fact(a-1); }
}

void main()
{
	int a;
	a = fact(3);
	sprint("Factorial of 3 is:\n");
	iprint(a);
	sprint("\n");
	return;
}