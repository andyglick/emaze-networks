package net.emaze.networks;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import org.junit.Assert;
import org.junit.Test;

public class SortNetworksByFirstThenLastIpTest {

    @Test
    public void sortingYieldsExpectedOrder() {
        final Collection<Network> unsorted = Arrays.asList(
                Network.fromCidrNotation("192.168.17.0/24"),
                Network.fromCidrNotation("192.168.0.0/24"),
                Network.fromCidrNotation("192.168.16.0/24"),
                Network.fromCidrNotation("192.168.1.0/24"));
        final List<Network> sorted = Arrays.asList(
                Network.fromCidrNotation("192.168.0.0/24"),
                Network.fromCidrNotation("192.168.1.0/24"),
                Network.fromCidrNotation("192.168.16.0/24"),
                Network.fromCidrNotation("192.168.17.0/24"));
        Assert.assertEquals(sorted, new SortNetworksByFirstThenLastIp().perform(unsorted));
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullCidrsThrows() {
        new SortNetworksByFirstThenLastIp().perform(null);
    }
}
