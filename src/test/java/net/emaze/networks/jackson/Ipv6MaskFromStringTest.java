package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import junit.framework.Assert;
import net.emaze.networks.Ipv6Mask;
import net.emaze.networks.jackson.Ipv6MaskToStringTest.BeanWithIpv6Mask;
import org.junit.Test;

public class Ipv6MaskFromStringTest {

    @Test
    public void deserializingYieldsExpected() throws IOException {
        final String serialized = "{'netmask':24}".replace("'", "\"");
        final Ipv6Mask expected = Ipv6Mask.net(24);
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithIpv6Mask got = mapper.readValue(serialized, BeanWithIpv6Mask.class);
        Assert.assertEquals(expected, got.getNetmask());
    }
}
