__asm__ (".symver xyz_1, xyz@test_1");

int xyz_1 (void)
{
	return 0;
};

void xyz (void){};
