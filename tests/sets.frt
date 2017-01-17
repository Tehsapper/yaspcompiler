double power(double x, int n)
{
	double result;
	int i;

	result = 1.0;
	
	for(i = 0; i < n; i = i + 1) { result = result * x; }
	return result;
}

int fact(int a)
{
	if(a <= 1) { return 1; }
	else { return a * fact(a-1); }
}

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