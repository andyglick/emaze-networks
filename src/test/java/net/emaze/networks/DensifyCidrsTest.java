package net.emaze.networks;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import org.junit.Assert;
import org.junit.Test;

public class DensifyCidrsTest {

    @Test
    public void contiguousCidrsOfSameNetmaskWillBeJoined() {
        final Cidr lhs = Cidr.parse("192.168.0.0/24");
        final Cidr rhs = Cidr.parse("192.168.1.0/24");
        final Set<Cidr> expected = Collections.singleton(Cidr.parse("192.168.0.0/23"));
        Assert.assertEquals(expected, new DensifyCidrs().perform(Arrays.asList(lhs, rhs)));
    }

    @Test
    public void noncontiguousCidrsWillNotBeJoined() {
        final Cidr lhs = Cidr.parse("192.168.0.0/24");
        final Cidr rhs = Cidr.parse("192.168.2.0/24");
        final Set<Cidr> expected = new HashSet<>(Arrays.asList(lhs, rhs));
        Assert.assertEquals(expected, new DensifyCidrs().perform(Arrays.asList(lhs, rhs)));
    }

    @Test
    public void canJoinNoncontiguousSetsOfContiguousCidrs() {
        final Collection<Cidr> toBeJoined = Arrays.asList(
                Cidr.parse("192.168.0.0/24"),
                Cidr.parse("192.168.1.0/24"),
                Cidr.parse("192.168.16.0/24"),
                Cidr.parse("192.168.17.0/24"));
        final Set<Cidr> expected = new HashSet<>(Arrays.asList(
                Cidr.parse("192.168.0.0/23"),
                Cidr.parse("192.168.16.0/23")));
        Assert.assertEquals(expected, new DensifyCidrs().perform(toBeJoined));
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void nullCidrsThrows(){
        new DensifyCidrs().perform(null);
    }
}
