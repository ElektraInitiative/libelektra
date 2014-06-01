#include <backends.hpp>

namespace kdb
{

namespace tools
{

std::vector<BackendInfo> Backends::getBackendInfo(KeySet mountConf)
{
	std::vector<BackendInfo> ret;
	Key rootKey (Backends::mountpointsPath, KEY_END);
	Key cur;

	mountConf.rewind();
	while (cur = mountConf.next())
	{
		if (rootKey.isDirectBelow(cur))
		{
			BackendInfo bi;

			Key path = mountConf.lookup (cur.getName() + "/config/path");
			if (path)
			{
				bi.path = path.getString();
			}
			Key mp = mountConf.lookup (cur.getName() + "/mountpoint");
			if (mp)
			{
				bi.mountpoint = mp.getString();
			}
			bi.name = cur.getBaseName();

			ret.push_back(bi);
		}
	}
	return std::move(ret);
}

const char *Backends::mountpointsPath = "system/elektra/mountpoints";

}

}
