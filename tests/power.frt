double power(double x, int n)
{
	double result;
	int i;

	result = 1.0;
	
	for(i = 0; i < n; i = i + 1) { result = result * x; }
	return result;
}
