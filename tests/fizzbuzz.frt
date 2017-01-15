void fizzbuzz(int i)
{
	if( i % 3 = 0) { sprint("Fizz"); }
	if( i % 5 = 0) { sprint("Buzz"); }
	if( i % 3 && i % 5) { iprint(i); }
	sprint("\n");
	return;
}

void main()
{
	int i;
	for ( i = 1; i <= 100; i = i + 1;)
	{
		fizzbuzz(i);
	}
	return;
}