package net.emaze.networks.old;

import net.emaze.networks.old.SortIpv4NetworksByFirstThenLastIp;
import net.emaze.networks.old.Ipv4Network;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import org.junit.Assert;
import org.junit.Test;

public class SortIpv4NetworksByFirstThenLastIpTest {

    @Test
    public void sortingYieldsExpectedOrder() {
        final Collection<Ipv4Network> unsorted = Arrays.asList(
                Ipv4Network.fromCidrNotation("192.168.17.0/24"),
                Ipv4Network.fromCidrNotation("192.168.0.0/24"),
                Ipv4Network.fromCidrNotation("192.168.16.0/24"),
                Ipv4Network.fromCidrNotation("192.168.1.0/24"));
        final List<Ipv4Network> sorted = Arrays.asList(
                Ipv4Network.fromCidrNotation("192.168.0.0/24"),
                Ipv4Network.fromCidrNotation("192.168.1.0/24"),
                Ipv4Network.fromCidrNotation("192.168.16.0/24"),
                Ipv4Network.fromCidrNotation("192.168.17.0/24"));
        Assert.assertEquals(sorted, new SortIpv4NetworksByFirstThenLastIp().perform(unsorted));
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullCidrsThrows() {
        new SortIpv4NetworksByFirstThenLastIp().perform(null);
    }
}
