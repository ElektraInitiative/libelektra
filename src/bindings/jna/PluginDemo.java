public class PluginDemo {
	int open(Key parentKey) {
		System.out.println("open plugin");
		return 0;
	}
	int get(KeySet ks, Key parentKey) {
		System.out.println("get plugin");
		return 0;
	}

	int set(KeySet ks, Key parentKey) {
		System.out.println("set plugin");
		return 0;
	}

	int close(Key parentKey) {
		System.out.println("close plugin");
		return 0;
	}

	public static void main(String [] args)
	{
		System.out.println("Hello World");
	}

}
