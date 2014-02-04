package net.emaze.networks.old;

import net.emaze.networks.old.DensifyIpv4Networks;
import net.emaze.networks.old.Ipv4Network;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import org.junit.Assert;
import org.junit.Test;

public class DensifyIpv4NetworksTest {

    @Test
    public void contiguousCidrsOfSameNetmaskWillBeJoined() {
        final Ipv4Network lhs = Ipv4Network.fromCidrNotation("192.168.0.0/24");
        final Ipv4Network rhs = Ipv4Network.fromCidrNotation("192.168.1.0/24");
        final Set<Ipv4Network> expected = Collections.singleton(Ipv4Network.fromCidrNotation("192.168.0.0/23"));
        Assert.assertEquals(expected, new DensifyIpv4Networks().perform(Arrays.asList(lhs, rhs)));
    }

    @Test
    public void noncontiguousCidrsWillNotBeJoined() {
        final Ipv4Network lhs = Ipv4Network.fromCidrNotation("192.168.0.0/24");
        final Ipv4Network rhs = Ipv4Network.fromCidrNotation("192.168.2.0/24");
        final Set<Ipv4Network> expected = new HashSet<>(Arrays.asList(lhs, rhs));
        Assert.assertEquals(expected, new DensifyIpv4Networks().perform(Arrays.asList(lhs, rhs)));
    }

    @Test
    public void canJoinNoncontiguousSetsOfContiguousCidrs() {
        final Collection<Ipv4Network> toBeJoined = Arrays.asList(
                Ipv4Network.fromCidrNotation("192.168.0.0/24"),
                Ipv4Network.fromCidrNotation("192.168.1.0/24"),
                Ipv4Network.fromCidrNotation("192.168.16.0/24"),
                Ipv4Network.fromCidrNotation("192.168.17.0/24"));
        final Set<Ipv4Network> expected = new HashSet<>(Arrays.asList(
                Ipv4Network.fromCidrNotation("192.168.0.0/23"),
                Ipv4Network.fromCidrNotation("192.168.16.0/23")));
        Assert.assertEquals(expected, new DensifyIpv4Networks().perform(toBeJoined));
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullCidrsThrows() {
        new DensifyIpv4Networks().perform(null);
    }
}
