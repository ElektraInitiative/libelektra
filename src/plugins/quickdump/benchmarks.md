# Benchmarks

## `benchmark_storage`

The following table shows a summary of the results of a `benchmark_storage` run:
NOTE: `factor` is always `dump / quickdump`

<table>
<thead><tr><th>operation</th>
<th>dump (µs)</th>
<th>quickdump (µs)</th>
<th>factor</th>
</tr></thead>
<tbody>
<tr>
<td>write keyset</td>
<td align="right">81661</td>
<td align="right">4741</td>
<td align="right">17.224</td>
</tr>
<tr>
<td>read keyset</td>
<td align="right">94782</td>
<td align="right">44673</td>
<td align="right">2.122</td>
</tr>
<tr>
<td>iterate keyset</td>
<td align="right">125</td>
<td align="right">135</td>
<td align="right">0.926</td>
</tr>
<tr>
<td>delete keyset</td>
<td align="right">1823</td>
<td align="right">3518</td>
<td align="right">0.518</td>
</tr>
<tr>
<td>re-read keyset</td>
<td align="right">92939</td>
<td align="right">43749</td>
<td align="right">2.124</td>
</tr>
<tr>
<td>strcmp key name</td>
<td align="right">609</td>
<td align="right">1826</td>
<td align="right">0.334</td>
</tr>
<tr>
<td>strcmp key value</td>
<td align="right">684</td>
<td align="right">761</td>
<td align="right">0.899</td>
</tr>
<tr>
</tbody>
</table>

## `benchmark_plugingetset`

A script was used to generate KeySets with `2`, `200`, `2,000`, `200,000` and `2,000,000` Keys. For keynames we used
increasing numbers (prefixed with `_` to keep correct order), each key had a randomized value of 15 characters.
Additionally we added 4 metakeys to each key. The names and values of these were created similarly.

We then used [`hyperfine`](https://github.com/sharkdp/hyperfine) to compare the performance of `dump` and `quickdump`.

```
hyperfine --min-runs 25 'benchmark_plugingetset <path> <parent> dump' 'benchmark_plugingetset <path> <parent> quickdump'
```

This means we used the same key sets for each of the runs, but the key values shouldn't really matter. It also means
that no shared metadata (`keyCopyMeta`) was involved.

We also used a modified version of `benchmark_plugingetset` that returned after the `get` call, to test the `get`
performance by itself. Because `set` doesn't scale linearly because of the checks for `keyCopyMeta`, the largest
KeySet was only used for the `get` only version. (The test was aborted after the initial run of `dump` took longer than
all the runs for `quickdump` together.)

The complete results can be viewed in the [benchmarks folder](benchmarks)

### get and set

The values are mean ± standard deviation. `factor` is `dump / quickdump` like above

<table class="table table-bordered table-hover table-condensed">
<thead><tr><th>no. of keys</th>
<th>dump (s)</th>
<th>quickdump (s)</th>
<th>factor</th>
</tr></thead>
<tbody><tr>
<td align="right">2</td>
<td align="right">0.0016 ± 0.0003</td>
<td align="right">0.0006 ± 0.0001</td>
<td align="right">2.67</td>
</tr>
<tr>
<td align="right">200</td>
<td align="right">0.0091 ± 0.0009</td>
<td align="right">0.0018 ± 0.0000</td>
<td align="right">5.06</td>
</tr>
<tr>
<td align="right">2000</td>
<td align="right">0.0770 ± 0.0030</td>
<td align="right">0.0127 ± 0.0008</td>
<td align="right">6.06</td>
</tr>
<tr>
<td align="right">200000</td>
<td align="right">10.7876 ± 4.2564</td>
<td align="right">1.2640 ± 0.0061</td>
<td align="right">8.53</td>
</tr>
</tbody></table>

### get only

The values are mean ± standard deviation. `factor` is `dump / quickdump` like above

<table>
<thead><tr><th title="Field #1">no. of keys</th>
<th>dump (s)</th>
<th>quickdump (s)</th>
<th>factor</th>
</tr></thead>
<tbody><tr>
<td align="right">2</td>
<td align="right">0.0014 ± 0.0000</td>
<td align="right">0.0005 ± 0.0000</td>
<td align="right">2.8</td>
</tr>
<tr>
<td align="right">200</td>
<td align="right">0.0032 ± 0.0005</td>
<td align="right">0.0013 ± 0.0002</td>
<td align="right">2.46</td>
</tr>
<tr>
<td align="right">2000</td>
<td align="right">0.0179 ± 0.0002</td>
<td align="right">0.0086 ± 0.0008</td>
<td align="right">2.08</td>
</tr>
<tr>
<td align="right">200000</td>
<td align="right">1.6830 ± 0.0222</td>
<td align="right">0.8515 ± 0.0048</td>
<td align="right">1.98</td>
</tr>
<tr>
<td align="right">2000000</td>
<td align="right">17.1814 ± 0.2201</td>
<td align="right">8.8808 ± 0.0344</td>
<td align="right">1.93</td>
</tr>
</tbody></table>
