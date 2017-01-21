double power(double x, int n);
int fact(int a);

double exp(double x) 
{
	double t;
	double result;
	int n;
	
	n = 0;
	t = power(x, n) / fact(n) -> double;
	result = 0.0;

	for(n = 1; t > 0.0001; n = n + 1 ) 
	{ 
		result = result + t; 
		t = power(x, n) / fact(n) -> double;
	}

	return result;
}

void main()
{
	sprint("e^2 = ");
	dprint(exp(2.0));
	sprint("\n");
	return;
}