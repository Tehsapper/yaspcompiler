void main()
{
	int i;
	i = 0;

	int f(int arg)
	{
		iprint(i); sprint("\n");
		if(i < arg)
		{
			i = i + 1;
			return f(arg - 1);
		}
		return i;
	}

	iprint(f(10)); sprint(": the last number\n");
	return;
}