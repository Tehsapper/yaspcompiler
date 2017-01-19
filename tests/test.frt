void main()
{
	int i;
	int j;

	for ( i = 0; i < 10; i = i + 1)
	{
		for ( j = 0; j < 10; j = j + 1)
		{
			if( i = 0 && j = 0) { continue; }
			iprint(j); sprint(" "); iprint(i); sprint("\n");
		}
	}

	while(1)
	{
		j = j + 1;
		iprint(i); sprint("\n");
		if ( j > 10 ) { break; }
	}

	return;
}